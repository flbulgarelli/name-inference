{-# LANGUAGE FlexibleInstances #-}

module Data.PersonalName (
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

import Data.Maybe (fromMaybe)

import Data.PersonalName.Class
import Data.PersonalName.Registry
import Data.PersonalName.Name

import qualified Data.Char as C

data PersonalName
  = GivenAndFamily [String] [String]
  | FullName [String]
  deriving (Eq, Show)

analyze :: Registry -> PersonalName -> (Name, Name)
analyze registry (GivenAndFamily given family) = (makeName registry given, makeName registry family)
analyze registry (FullName names)              = splitNames . map (makeSingletonName registry) $ names

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

