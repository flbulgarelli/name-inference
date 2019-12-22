module Data.PersonalName.Name (
  bonusFamilish,
  bonusGivenish,
  breakNames,
  justBreakNames,
  justBreakNamesWith,
  makeName,
  makeSingletonName,
  mergeNames,
  namesConfidence,
  noBonus,
  splitNames,
  splitNamesWith,
  swapNames,
  Name (..),
  NameDivider,
  ConfidenceBonus) where

import Data.Function (on)
import Data.List (splitAt, find, maximumBy)
import Data.PersonalName.Class (ambiguousCenter, confidence, merge, isFamilish, isGivenish, Class(..))

import Data.PersonalName.Registry (classify, classifyMany, Registry)

data Name = Name { ns :: [String], cls :: Class } deriving (Eq, Show)
type NameDivider = [Name] -> Maybe (Name, Name)
type ConfidenceBonus = [String] -> [String] -> Int

mergeNames :: [Name] -> Name
mergeNames names = Name (concatMap ns names) (merge . map cls $ names)

namesConfidence :: ConfidenceBonus -> (Name, Name) -> Int
namesConfidence bonus names@(n1, n2) = adjustConfidence (confidence (cls n1, cls n2)) bonus names

splitNamesWith :: ConfidenceBonus -> NameDivider
splitNamesWith _     names | ambiguousNamesCenter names = Nothing
splitNamesWith bonus names = Just $ breakNames bonus names

splitNames :: NameDivider
splitNames = splitNamesWith noBonus

justBreakNamesWith :: ConfidenceBonus -> NameDivider
justBreakNamesWith bonus = Just . breakNames bonus

justBreakNames :: NameDivider
justBreakNames = justBreakNamesWith noBonus

ambiguousNamesCenter :: [Name] -> Bool
ambiguousNamesCenter = ambiguousCenter . map cls

breakNames :: ConfidenceBonus -> [Name] -> (Name, Name)
breakNames bonus = maximumBy (compare `on` (namesConfidence bonus)) . map (swapNames . mergePartition) . partitions
  where
    mergePartition (start, end)     = (mergeNames start, mergeNames end)

makeName :: Registry -> [String] -> Name
makeName registry n = Name n (classifyMany registry n)

makeSingletonName :: Registry -> String -> Name
makeSingletonName registry n = Name [n] (classify registry n)


partitions :: [a] -> [([a], [a])]
partitions xs = [splitAt x xs| x <- [1 .. (length xs - 1)]]

adjustConfidence :: Int -> ConfidenceBonus -> (Name, Name) ->  Int
adjustConfidence n bonus (Name ns1 cls1, Name ns2 cls2) | isGivenish cls1 && isFamilish cls2 =  9 * n + bonus ns1 ns2
adjustConfidence n _     _                              = n

bonusGivenish :: ConfidenceBonus
bonusGivenish gs fs | length gs > length fs = 5
bonusGivenish gs fs | length gs < length fs = -5
bonusGivenish _  _  = 0

bonusFamilish :: ConfidenceBonus
bonusFamilish gs fs | length gs < length fs = 5
bonusFamilish gs fs | length gs > length fs = -5
bonusFamilish _  _  = 0

noBonus :: ConfidenceBonus
noBonus _ _ = 0

swapNames :: (Name, Name) -> (Name, Name)
swapNames (f@(Name _ Family), g@(Name _ cls  )) | isGivenish cls = (g, f)
swapNames (f@(Name _ cls   ), g@(Name _ Given)) | isFamilish cls = (g, f)
swapNames names                                 = names
