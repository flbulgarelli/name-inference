{-# LANGUAGE FlexibleInstances #-}

module Data.PersonalName (
  analyze,
  defaultOptions,
  fix,
  fixMaybe,
  justBreakNames,
  makeRegistry,
  splitNames,
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
analyze registry _       (GivenAndFamily given family) = Just (makeName registry given, makeName registry family)
analyze registry divider (FullName names)              = divider . map (makeSingletonName registry) $ names

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
makePersonalName (Name _ Given)  (Name _ Bad)     = Nothing
makePersonalName (Name g Given)  (Name f _)       = Just $ GivenAndFamily g f
makePersonalName (Name g Family) (Name f Family)  = Nothing
makePersonalName (Name g Family) (Name f Bad)     = Nothing
makePersonalName (Name g Family) (Name f _)       = Just $ GivenAndFamily f g
makePersonalName (Name g Other)  (Name f Given)   = Just $ GivenAndFamily f g
makePersonalName (Name g Other)  (Name f Family)  = Just $ GivenAndFamily g f
makePersonalName (Name g Other)  (Name f Other)   = Just $ GivenAndFamily g f
makePersonalName _               _                = Nothing

