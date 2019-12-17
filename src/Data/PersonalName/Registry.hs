module Data.PersonalName.Registry (
  classify,
  classifyMany,
  defaultOptions,
  encode,
  encodeMany,
  makeRegistry,
  registeredAsAmbiguous,
  registeredAsFamily,
  registeredAsGiven,
  Registry (..),
  RegistryOptions (..)) where

import Data.Set (Set, fromList, intersection, difference)
import Data.Text (Text, pack, unpack, toLower)
import Text.Inflections (transliterate)

import Data.PersonalName.Class (merge, Class(..))


data Registry = Registry {
    givens :: Set Text,
    families :: Set Text,
    ambiguous :: Set Text,
    options :: RegistryOptions
  } deriving (Eq, Show)

data RegistryOptions = RegistryOptions {
    transliterateNames :: Bool,
    treatUnknownAsFamily :: Bool
  } deriving (Eq, Show)


defaultOptions :: RegistryOptions
defaultOptions = RegistryOptions False False

registeredAsGiven, registeredAsFamily, registeredAsAmbiguous :: Text -> Registry -> Bool
registeredAsGiven n = elem n . givens
registeredAsFamily n = elem n . families
registeredAsAmbiguous n = elem n . ambiguous

encode :: RegistryOptions -> String -> Text
encode registry n | transliterateNames registry = transliterate . toLower . pack $ n
encode _        n = toLower . pack $ n

encodeMany :: RegistryOptions -> [String] -> Set Text
encodeMany options = fromList . map (encode options)

classify :: Registry -> String -> Class
classify registry n
  | registeredAsGiven n' registry                 = Given
  | registeredAsFamily n' registry                = Family
  | registeredAsAmbiguous n' registry             = Other
  | treatUnknownAsFamily . options $ registry     = Family
  | otherwise                                     = Other
    where n' = encode (options registry) n


classifyMany :: Registry -> [String] -> Class
classifyMany registry = merge . map (classify registry)

makeRegistry :: [String] -> [String] -> RegistryOptions -> Registry
makeRegistry gs fs options = Registry (difference givens ambiguous) (difference families ambiguous) ambiguous options
  where
    givens = encodeMany options gs
    families = encodeMany options fs
    ambiguous = intersection givens families

