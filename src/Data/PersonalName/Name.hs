module Data.PersonalName.Name (
  mergeNames,
  namesConfidence,
  splitNames,
  makeName,
  makeSingletonName,
  Name (..)) where

import Data.Function (on)
import Data.List (splitAt, find, maximumBy)
import Data.PersonalName.Class (confidence, merge, Class)

import Data.PersonalName.Registry (classify, classifyMany, Registry)


data Name = Name { ns :: [String], cls :: Class } deriving (Eq, Show)

mergeNames :: [Name] -> Name
mergeNames names = Name (concatMap ns names) (merge . map cls $ names)

namesConfidence :: (Name, Name) -> Int
namesConfidence (n1, n2) = confidence (cls n1, cls n2)

splitNames :: [Name] -> (Name, Name)
splitNames = maximumBy (compare `on` namesConfidence) . map mergePartition . partitions
  where
    mergePartition (start, end)     = (mergeNames start, mergeNames end)

makeName :: Registry -> [String] -> Name
makeName registry n = Name n (classifyMany registry n)

makeSingletonName :: Registry -> String -> Name
makeSingletonName registry n = Name [n] (classify registry n)


partitions :: [a] -> [([a], [a])]
partitions xs = [splitAt x xs| x <- [1 .. (length xs - 1)]]
