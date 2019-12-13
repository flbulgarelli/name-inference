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
  | Impossible
  deriving (Show, Eq)

data Name
  = FullName [String]
  | NameAndSurname [String] [String]
  deriving (Eq, Show)

data FixedName = FixedName (Uncertain [String]) (Uncertain [String]) deriving (Eq, Show)

data Class = N | S | A | UN | US | I deriving Eq

classify :: Bag -> String -> Class
classify bag n
  | inNames' n bag && not (inSurnames' n bag) = N
  | inSurnames' n bag && not (inNames' n bag) = S
  | otherwise                         = A

classifyMany :: Bag -> [String] -> Class
classifyMany bag =  foldClass . map (classify bag)


foldClass :: [Class] -> Class
foldClass xs@(c:cs)   = foo c (foldMiddleClass c cs) (lasthy xs)

foo h m l | m /= I && h /= l = un h
foo h m l = m

un N = UN
un S = US
un A = A

foldMiddleClass c []     = c
foldMiddleClass A (c:cs) = foldMiddleClass c cs
foldMiddleClass r [A]    = r
foldMiddleClass r [c]    | r == c = r
foldMiddleClass _ [_]    = I
foldMiddleClass r (c:cs) | foldMiddleClass r cs == r = r
foldMiddleClass _ _      = I

lasthy [x] = x
lasthy xs  = last xs

confidence :: FixedName -> Int
confidence (FixedName (Sure _)   (Sure _))   = 2
confidence (FixedName _          (Sure _))   = 1
confidence (FixedName (Sure _)   _       )   = 1
confidence (FixedName (Unsure _) _       )   = 0
confidence (FixedName _          (Unsure _)) = 0
confidence (FixedName _          _         ) = -1

fix :: Bag -> Name -> FixedName
fix bag = maximumBy (compare `on` confidence) . fixes bag

fixes :: Bag -> Name -> [FixedName]
fixes bag (NameAndSurname tentativeName tentativeSurname) = [fix' bag tentativeName tentativeSurname]
fixes bag (FullName tenatives)                            = map (uncurry (fix' bag)) . partitions $ tenatives

fix' :: Bag -> [String] -> [String] -> FixedName
fix' bag tentativeName tentativeSurname = fixClass (classifyMany bag tentativeName) (classifyMany bag tentativeSurname)
  where
    fixClass N N = FixedName Impossible Impossible
    fixClass N UN = FixedName Impossible Impossible
    fixClass N S = FixedName (Sure tentativeName) (Sure tentativeSurname)
    fixClass N US = FixedName (Sure tentativeName) (Unsure tentativeSurname)
    fixClass N A = FixedName (Sure tentativeName) (Unsure tentativeSurname)
    fixClass N I = FixedName (Sure tentativeName) Impossible
    fixClass UN N = FixedName Impossible Impossible
    fixClass UN UN = FixedName Impossible Impossible
    fixClass UN S = FixedName (Unsure tentativeName) (Sure tentativeSurname)
    fixClass UN US = FixedName (Unsure tentativeName) (Unsure tentativeSurname)
    fixClass UN A = FixedName (Unsure tentativeName) (Unsure tentativeSurname)
    fixClass UN I = FixedName (Unsure tentativeName) Impossible
    fixClass S N = FixedName (Sure tentativeSurname) (Sure tentativeName)
    fixClass S UN = FixedName (Sure tentativeSurname) (Unsure tentativeName)
    fixClass S S = FixedName Impossible Impossible
    fixClass S US = FixedName Impossible Impossible
    fixClass S A = FixedName (Unsure tentativeSurname) (Sure tentativeName)
    fixClass S I = FixedName Impossible (Sure tentativeName)
    fixClass US N = FixedName (Sure tentativeSurname) (Unsure tentativeName)
    fixClass US UN = FixedName (Unsure tentativeSurname) (Unsure tentativeName)
    fixClass US S = FixedName Impossible Impossible
    fixClass US US = FixedName Impossible Impossible
    fixClass US A = FixedName (Unsure tentativeSurname) (Unsure tentativeName)
    fixClass US I = FixedName Impossible (Unsure tentativeName)
    fixClass A N = FixedName (Sure tentativeSurname) (Unsure tentativeName)
    fixClass A UN = FixedName (Unsure tentativeSurname) (Unsure tentativeName)
    fixClass A S = FixedName (Unsure tentativeName) (Sure tentativeSurname)
    fixClass A US = FixedName (Unsure tentativeName) (Unsure tentativeSurname)
    fixClass A A = FixedName (Unsure tentativeName) (Unsure tentativeSurname)
    fixClass A I = FixedName (Unsure tentativeName) Impossible
    fixClass I N = FixedName (Sure tentativeSurname) Impossible
    fixClass I UN = FixedName (Unsure tentativeSurname) Impossible
    fixClass I S = FixedName Impossible (Sure tentativeSurname)
    fixClass I US = FixedName Impossible (Unsure tentativeSurname)
    fixClass I A = FixedName Impossible (Unsure tentativeSurname)
    fixClass I I = FixedName Impossible Impossible

partitions :: [a] -> [([a], [a])]
partitions xs = [splitAt x xs| x <- [1 .. (length xs - 1)]]

inNames    :: [String] -> Bag -> Bool
inNames    ns bag = all (\n -> inNames' n bag || not (inSurnames' n bag)) (middle ns) && all (`inNames'` bag) (extremes ns)

inSurnames :: [String] -> Bag -> Bool
inSurnames ns bag = all (\n -> inSurnames' n bag || not (inNames' n bag)) (middle ns) && all (`inSurnames'` bag) (extremes ns)

inNames'    n = elem n . names
inSurnames' n = elem n . surnames

isName, isSurname, isAmbiguous :: [String] -> Bag -> Bool
isName      n bag = inNames n bag && not (inSurnames n bag)
isSurname   n bag = inSurnames n bag && not (inNames n bag)
isAmbiguous n bag = inSurnames n bag && inNames n bag || not (inSurnames n bag) && not (inNames n bag)

middle (_:x:xs) = tail (x:xs)
middle _        = []

extremes (x:y:xs) = [x, last (y:xs)]
extremes (x:_)    = [x]
