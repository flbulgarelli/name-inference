{-# LANGUAGE FlexibleInstances #-}

module Data.Name (
  fix,
  Bag (..),
  Name (..),
  Uncertain (..),
  FixedName (..)) where

import Data.List (splitAt)

data Bag = Bag { names :: [String], surnames :: [String] } deriving Show

class NamePart a where
  inNames    :: a -> Bag -> Bool
  inSurnames :: a -> Bag -> Bool

  isName, isSurname, isAmbiguous :: a -> Bag -> Bool
  isName      n bag = inNames n bag && not (inSurnames n bag)
  isSurname   n bag = inSurnames n bag && not (inNames n bag)
  isAmbiguous n bag = inSurnames n bag && inNames n bag || not (inSurnames n bag) && not (inNames n bag)

instance NamePart String where
  inNames    n = elem n . names
  inSurnames n = elem n . surnames

instance NamePart [String] where
    inNames    ns bag = all (\n -> inNames n bag || not (inSurnames n bag)) ns
    inSurnames ns bag = all (\n -> inSurnames n bag || not (inNames n bag)) ns

data Uncertain a
  = Sure a
  | Unsure a
  deriving (Show, Eq)

data Name
  = FullName [String]
  | NameAndSurname [String] [String]
  deriving (Eq, Show)

data FixedName = FixedName (Uncertain [String]) (Uncertain [String]) deriving (Eq, Show)


fix bag (NameAndSurname tentativeName tentativeSurname)
  | isName tentativeName       bag && isSurname tentativeSurname   bag = FixedName (Sure tentativeName) (Sure tentativeSurname)
  | isName tentativeSurname    bag && isSurname tentativeName      bag = FixedName (Sure tentativeSurname) (Sure tentativeName)
  | isName tentativeName       bag && isAmbiguous tentativeSurname bag = FixedName (Sure tentativeName) (Unsure tentativeSurname)
  | isName tentativeSurname    bag && isAmbiguous tentativeName    bag = FixedName (Sure tentativeSurname) (Unsure tentativeName)
  | isSurname tentativeName    bag && isAmbiguous tentativeSurname bag = FixedName (Unsure tentativeSurname) (Sure tentativeName)
  | isSurname tentativeSurname bag && isAmbiguous tentativeName    bag = FixedName (Unsure tentativeName) (Sure tentativeSurname)
  | otherwise = FixedName (Unsure tentativeName) (Unsure tentativeSurname)

partitions xs = [splitAt x xs| x <- [1 .. (length xs - 1)]]

