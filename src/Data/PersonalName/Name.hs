module Data.PersonalName.Name (
  breakNames,
  justBreakNames,
  makeName,
  makeSingletonName,
  mergeNames,
  namesConfidence,
  splitNames,
  Name (..),
  NameDivider) where

import Data.Function (on)
import Data.List (splitAt, find, maximumBy)
import Data.PersonalName.Class (ambiguousCenter, confidence, merge, Class)

import Data.PersonalName.Registry (classify, classifyMany, Registry)


data Name = Name { ns :: [String], cls :: Class } deriving (Eq, Show)
type NameDivider = [Name] -> Maybe (Name, Name)

mergeNames :: [Name] -> Name
mergeNames names = Name (concatMap ns names) (merge . map cls $ names)

namesConfidence :: (Name, Name) -> Int
namesConfidence (n1, n2) = confidence (cls n1, cls n2)

splitNames :: NameDivider
splitNames names | ambiguousNamesCenter names = Nothing
splitNames names = Just $ breakNames names

justBreakNames :: NameDivider
justBreakNames = Just . breakNames

ambiguousNamesCenter :: [Name] -> Bool
ambiguousNamesCenter = ambiguousCenter . map cls

breakNames :: [Name] -> (Name, Name)
breakNames = maximumBy (compare `on` namesConfidence) . map mergePartition . partitions
  where
    mergePartition (start, end)     = (mergeNames start, mergeNames end)

makeName :: Registry -> [String] -> Name
makeName registry n = Name n (classifyMany registry n)

makeSingletonName :: Registry -> String -> Name
makeSingletonName registry n = Name [n] (classify registry n)


partitions :: [a] -> [([a], [a])]
partitions xs = [splitAt x xs| x <- [1 .. (length xs - 1)]]
