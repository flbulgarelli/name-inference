{-# LANGUAGE FlexibleInstances #-}

module Data.Name (
  fix,
  fixes,
  confidence,
  Bag (..),
  Name (..),
  Uncertain (..),
  FixedName (..)) where

import Data.Function (on)
import Data.List (splitAt, maximumBy)

data Bag = Bag { names :: [String], surnames :: [String] } deriving Show

data Uncertain a
  = Sure a
  | Unsure a
  deriving (Show, Eq)

data Name
  = FullName [String]
  | NameAndSurname [String] [String]
  deriving (Eq, Show)

data FixedName = FixedName (Uncertain [String]) (Uncertain [String]) deriving (Eq, Show)

confidence :: FixedName -> Int
confidence (FixedName (Sure _) (Sure _))  = 2
confidence (FixedName _        (Sure _))  = 1
confidence (FixedName (Sure _) _       )  = 1
confidence _                              = 0

fix :: Bag -> Name -> FixedName
fix bag = maximumBy (compare `on` confidence) . fixes bag

fixes :: Bag -> Name -> [FixedName]
fixes bag (NameAndSurname tentativeName tentativeSurname) = [fix' bag tentativeName tentativeSurname]
fixes bag (FullName tenatives)                            = map (uncurry (fix' bag)) . partitions $ tenatives

fix' :: Bag -> [String] -> [String] -> FixedName
fix' bag tentativeName tentativeSurname
  | isName tentativeName       bag && isSurname tentativeSurname   bag = FixedName (Sure tentativeName) (Sure tentativeSurname)
  | isName tentativeSurname    bag && isSurname tentativeName      bag = FixedName (Sure tentativeSurname) (Sure tentativeName)
  | isName tentativeName       bag && isAmbiguous tentativeSurname bag = FixedName (Sure tentativeName) (Unsure tentativeSurname)
  | isName tentativeSurname    bag && isAmbiguous tentativeName    bag = FixedName (Sure tentativeSurname) (Unsure tentativeName)
  | isSurname tentativeName    bag && isAmbiguous tentativeSurname bag = FixedName (Unsure tentativeSurname) (Sure tentativeName)
  | isSurname tentativeSurname bag && isAmbiguous tentativeName    bag = FixedName (Unsure tentativeName) (Sure tentativeSurname)
  | otherwise                                                          = FixedName (Unsure tentativeName) (Unsure tentativeSurname)

partitions :: [a] -> [([a], [a])]
partitions xs = [splitAt x xs| x <- [1 .. (length xs - 1)]]

inNames    :: [String] -> Bag -> Bool
inNames    ns bag = all (\n -> inNames' n bag || not (inSurnames' n bag)) ns

inSurnames :: [String] -> Bag -> Bool
inSurnames ns bag = all (\n -> inSurnames' n bag || not (inNames' n bag)) ns

inNames'    n = elem n . names
inSurnames' n = elem n . surnames

isName, isSurname, isAmbiguous :: [String] -> Bag -> Bool
isName      n bag = inNames n bag && not (inSurnames n bag)
isSurname   n bag = inSurnames n bag && not (inNames n bag)
isAmbiguous n bag = inSurnames n bag && inNames n bag || not (inSurnames n bag) && not (inNames n bag)
