{-# LANGUAGE FlexibleInstances #-}

module Data.PersonalName (
  analyze,
  bonusFamilish,
  bonusGivenish,
  defaultOptions,
  fix,
  fixMaybe,
  justBreakNames,
  justBreakNamesWith,
  makeRegistry,
  noBonus,
  splitNames,
  splitNamesWith,
  ConfidenceBonus,
  Class(..),
  Name(..),
  NameDivider,
  Registry (..),
  RegistryOptions (..),
  PersonalName (..)) where

import Data.Maybe (fromMaybe)

import Data.PersonalName.Class
import Data.PersonalName.Registry
import Data.PersonalName.Name

import qualified Data.Char as C


data PersonalName
  = GivenAndFamily [String] [String]
  | FullName [String]
  deriving (Eq, Show)

analyze :: Registry -> NameDivider -> PersonalName -> Maybe (Name, Name)
analyze registry _           (GivenAndFamily given family) = Just . swapNames $ (makeName registry given, makeName registry family)
analyze registry divideNames (FullName names)              = divideNames . map (makeSingletonName registry) $ names

fixMaybe :: Registry -> NameDivider -> PersonalName -> Maybe PersonalName
fixMaybe _        _        (FullName [])         = Nothing
fixMaybe _        _        (FullName [_])        = Nothing
fixMaybe _        _        (GivenAndFamily [] _) = Nothing
fixMaybe _        _        (GivenAndFamily _ []) = Nothing
fixMaybe registry divider  name                  = analyze registry divider name >>= uncurry makePersonalName

fix :: Registry -> NameDivider -> PersonalName -> PersonalName
fix registry divider n = fromMaybe n . fixMaybe registry divider $ n

makePersonalName :: Name -> Name -> Maybe PersonalName
makePersonalName (Name _ Given)  (Name _ Given)   = Nothing
makePersonalName (Name _ Family) (Name _ Family)  = Nothing
-- makePersonalName (Name _ Other)  (Name _ Other)  = Nothing
makePersonalName _               (Name _ Bad)     = Nothing
makePersonalName (Name _ Bad)    _                = Nothing
makePersonalName (Name g _)     (Name f _)       = Just $ GivenAndFamily g f

