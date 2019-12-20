module Data.PersonalName.Name (
  breakNames,
  justBreakNames,
  makeName,
  makeSingletonName,
  mergeNames,
  namesConfidence,
  splitNames,
  swapNames,
  Name (..),
  NameDivider) where

import Data.Function (on)
import Data.List (splitAt, find, maximumBy)
import Data.PersonalName.Class (ambiguousCenter, confidence, merge, isFamilish, isGivenish, Class(..))

import Data.PersonalName.Registry (classify, classifyMany, Registry)


data Name = Name { ns :: [String], cls :: Class } deriving (Eq, Show)
type NameDivider = [Name] -> Maybe (Name, Name)

mergeNames :: [Name] -> Name
mergeNames names = Name (concatMap ns names) (merge . map cls $ names)

namesConfidence :: (Name, Name) -> Int
namesConfidence names@(n1, n2) = adjustConfidence (confidence (cls n1, cls n2)) noBonus names

splitNames :: NameDivider
splitNames names | ambiguousNamesCenter names = Nothing
splitNames names = Just $ breakNames names

justBreakNames :: NameDivider
justBreakNames = Just . breakNames

ambiguousNamesCenter :: [Name] -> Bool
ambiguousNamesCenter = ambiguousCenter . map cls

breakNames :: [Name] -> (Name, Name)
breakNames = maximumBy (compare `on` namesConfidence) . map (swapNames . mergePartition) . partitions
  where
    mergePartition (start, end)     = (mergeNames start, mergeNames end)

makeName :: Registry -> [String] -> Name
makeName registry n = Name n (classifyMany registry n)

makeSingletonName :: Registry -> String -> Name
makeSingletonName registry n = Name [n] (classify registry n)


partitions :: [a] -> [([a], [a])]
partitions xs = [splitAt x xs| x <- [1 .. (length xs - 1)]]



type Bonus = [String] -> [String] -> Int

adjustConfidence :: Int -> Bonus -> (Name, Name) ->  Int
adjustConfidence n bonus (Name ns1 cls1, Name ns2 cls2) | isGivenish cls1 && isFamilish cls2 =  9 * n + bonus ns1 ns2
adjustConfidence n _     _                              = 9 * n

bonusGivenish :: Bonus
bonusGivenish gs fs | length gs > length fs = 5
bonusGivenish gs fs | length gs < length fs = -5
bonusGivenish _  _  = 0

bonusFamilish :: Bonus
bonusFamilish gs fs | length gs < length fs = 5
bonusFamilish gs fs | length gs > length fs = -5
bonusFamilish _  _  = 0

noBonus :: Bonus
noBonus _ _ = 0

swapNames :: (Name, Name) -> (Name, Name)
swapNames (f@(Name _ Family), g@(Name _ cls  )) | isGivenish cls = (g, f)
swapNames (f@(Name _ cls   ), g@(Name _ Given)) | isFamilish cls = (g, f)
swapNames names                                 = names
