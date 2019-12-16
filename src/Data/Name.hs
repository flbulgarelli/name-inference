{-# LANGUAGE FlexibleInstances #-}

module Data.Name (
  fix,
  fixMaybe,
  analyze,
  makeRegistry,
  defaultOptions,
  Class(..),
  Name(..),
  Registry (..),
  RegistryOptions (..),
  PersonalName (..)) where

import Data.Function (on)
import Data.List (splitAt, find, maximumBy)
import Data.Maybe (fromMaybe)
import Data.Set (Set, fromList, intersection, difference)
import Data.Text (pack, unpack, toLower)
import Text.Inflections (transliterate)

import qualified Data.Char as C

data Registry = Registry {
    givens :: Set String,
    families :: Set String,
    ambiguous :: Set String,
    options :: RegistryOptions
  } deriving (Eq, Show)

data RegistryOptions = RegistryOptions {
    transliterateNames :: Bool,
    treatUnknownAsFamily :: Bool
  } deriving (Eq, Show)

data PersonalName
  = GivenAndFamily [String] [String]
  | FullName [String]
  deriving (Eq, Show)

data Class = Given | Family | Other | Bad deriving (Eq, Show)

data Name = Name { ns :: [String], cls :: Class } deriving (Eq, Show)

-- Classes functions

isGivenish, isFamilish :: Class -> Bool

isGivenish Given = True
isGivenish Other = True
isGivenish _     = False

isFamilish Family = True
isFamilish Other = True
isFamilish _     = False

merge :: [Class] -> Class
merge [x]    = x
merge (Given:xs)  | all isGivenish xs = Given
merge (Family:xs) | all isFamilish xs = Family
merge (Other:xs)  = merge xs
merge _           = Bad

confidence :: (Class, Class) -> Int
confidence (Given, Family)  = 8
confidence (Family, Given)  = 8
confidence (Family, Other)  = 7
confidence (Other, Family)  = 7
confidence (Given, Other)   = 6
confidence (Other, Given)   = 6
confidence (Other, Other)   = 5
confidence (Family, Family) = 4
confidence (Given, Given)   = 4
confidence (Bad, Family)    = 3
confidence (Family, Bad)    = 3
confidence (Bad, Given)     = 2
confidence (Given, Bad)     = 2
confidence (Bad, Other)     = 1
confidence (Other, Bad)     = 1
confidence (Bad, Bad)       = 0

-- Names functions

mergeNames :: [Name] -> Name
mergeNames names = Name (concatMap ns names) (merge . map cls $ names)

namesConfidence :: (Name, Name) -> Int
namesConfidence (n1, n2) = confidence (cls n1, cls n2)

splitNames :: [Name] -> (Name, Name)
splitNames = maximumBy (compare `on` namesConfidence) . map mergePartition . partitions
  where
    mergePartition (start, end)     = (mergeNames start, mergeNames end)

partitions :: [a] -> [([a], [a])]
partitions xs = [splitAt x xs| x <- [1 .. (length xs - 1)]]

-- Registry functions

defaultOptions :: RegistryOptions
defaultOptions = RegistryOptions False False

registeredAsGiven, registeredAsFamily :: String -> Registry -> Bool
registeredAsGiven n = elem n . givens
registeredAsFamily n = elem n . families
registeredAsAmbiguous n = elem n . ambiguous

-- Classification functions

classify :: Registry -> String -> Class
classify registry n
  | registeredAsGiven n' registry                 = Given
  | registeredAsFamily n' registry                = Family
  | registeredAsAmbiguous n' registry             = Other
  | treatUnknownAsFamily . options $ registry     = Family
  | otherwise                                     = Other
    where n' = encode (options registry) n

classifyMany :: Registry -> [String] -> Class
classifyMany registry = merge . map (classify registry)

toName :: Registry -> [String] -> Name
toName registry n = Name n (classifyMany registry n)

toSingletonName :: Registry -> String -> Name
toSingletonName registry n = Name [n] (classify registry n)

makeRegistry :: [String] -> [String] -> RegistryOptions -> Registry
makeRegistry gs fs options = Registry (difference givens ambiguous) (difference families ambiguous) ambiguous options
  where
    givens = encodeMany options gs
    families = encodeMany options fs
    ambiguous = intersection givens families

encode :: RegistryOptions -> String -> String
encode registry n | transliterateNames registry = unpack . transliterate . toLower . pack $ n
encode _        n = map C.toLower n

encodeMany :: RegistryOptions -> [String] -> Set String
encodeMany options = fromList . map (encode options)

-- Fix functions

analyze :: Registry -> PersonalName -> (Name, Name)
analyze registry (GivenAndFamily given family) = (toName registry given, toName registry family)
analyze registry (FullName names)              = splitNames . map (toSingletonName registry) $ names

fixMaybe :: Registry -> PersonalName -> Maybe PersonalName
fixMaybe registry = uncurry makePersonalName . analyze registry

fix :: Registry -> PersonalName -> PersonalName
fix registry n = fromMaybe n . fixMaybe registry $ n

makePersonalName :: Name -> Name -> Maybe PersonalName
makePersonalName (Name _ Given)  (Name _ Given)   = Nothing
makePersonalName (Name _ Given)  (Name _ Bad)     = Nothing
makePersonalName (Name g Given)  (Name f _)       = Just $ GivenAndFamily g f
makePersonalName (Name g Family) (Name f Family)  = Nothing
makePersonalName (Name g Family) (Name f Bad)     = Nothing
makePersonalName (Name g Family) (Name f _)       = Just $ GivenAndFamily f g
makePersonalName (Name g Other)  (Name f Given)   = Just $ GivenAndFamily f g
makePersonalName (Name g Other)  (Name f Family)  = Just $ GivenAndFamily g f
makePersonalName (Name g Other)  (Name f Other)   = Just $ GivenAndFamily g f
makePersonalName _               _                = Nothing

