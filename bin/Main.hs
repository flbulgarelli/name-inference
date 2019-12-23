{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Options.Applicative
import Data.Semigroup ((<>))
import Data.Foldable (for_)
import Data.Text as T (Text, pack, unpack, splitOn, strip, replace, toTitle, null)
import Data.List (intercalate)
import Data.PersonalName
import Data.PersonalName.Name (splitNamesWith, justBreakNamesWith, noBonus, bonusFamilish, bonusGivenish, ConfidenceBonus)

data Input = Input {
    givensFile :: String,
    familiesFile :: String,
    inputFile :: String,
    outputFormat :: String,
    confidenceBonus :: String,
    transliterate :: Bool,
    unknownAsFamily :: Bool,
    breakFullNames :: Bool
  }

sample :: Parser Input
sample = Input
      <$> strOption (
        long "givens"
         <> short 'g'
         <> metavar "FILE"
         <> help "Givens filename" )
      <*> strOption (
        long "families"
         <> short 'f'
         <> metavar "FILE"
         <> help "Families filename" )
      <*> strOption (
        long "file"
         <> short 'F'
         <> metavar "FILE"
         <> value "--"
         <> help "Input filename" )
      <*> strOption (
        long "output-format"
         <> short 'o'
         <> metavar "FORMAT"
         <> value "tagged"
         <> help "Output format {tagged|csv|padded}. Default is 'tagged'" )
      <*> strOption (
        long "bonus"
          <> short 'B'
          <> metavar "BONUS"
          <> value "no"
          <> help "Try to maximize length of a name group. Options are {no|given|family}. Default is 'no'" )
      <*> switch (
        long "transliterate"
         <> short 't'
         <> help "Transliterate names before classifying them" )
      <*> switch (
        long "unknown-as-family"
         <> short 'u'
         <> help "Treat unknown names as family names")
      <*> switch (
        long "break-full-names"
         <> short 'b'
         <> help "Force split of ambiguous full names")

main :: IO ()
main = execParser opts >>= run
  where
    opts = info (sample <**> helper) (fullDesc <> progDesc "Deterministic classifier for personal names")

run :: Input -> IO ()
run (Input givens families file outputFormat bonus transliterate unknownAsFamily breakFullNames) = do
  givens <- fmap lines (readFile givens)
  families <- fmap lines (readFile families)
  let registry = makeRegistry givens families (RegistryOptions transliterate unknownAsFamily)
  contents <- if file == "--" then getContents else readFile file
  for_ (lines contents) (processLine registry (selectFormat outputFormat) (selectDivider breakFullNames bonus))

selectDivider :: Bool -> String -> NameDivider
selectDivider break bonus = divider (selectBonus bonus)
  where
    divider = if break then justBreakNamesWith else splitNamesWith

selectBonus :: String -> ConfidenceBonus
selectBonus "given"  = bonusGivenish
selectBonus "family" = bonusFamilish
selectBonus _        = noBonus

processLine :: Registry -> Format ->  NameDivider -> String -> IO ()
processLine registry format divider = putStrLn  . format . fix registry divider . FullName . map unpack . prepare . pack

type Format = PersonalName -> String

selectFormat :: String -> Format
selectFormat "tagged" = tagged
selectFormat "csv"    = csv
selectFormat _        = padded

tagged :: Format
tagged (FullName names)       = "FullName:" ++ intercalate " " names
tagged (GivenAndFamily gs fs) = "GivenAndFamily:" ++ intercalate " " gs ++ "," ++ intercalate " " fs

csv :: Format
csv (FullName names)       = intercalate " " names
csv (GivenAndFamily gs fs) = intercalate " " gs ++ "," ++ intercalate " " fs

padded :: Format
padded (FullName names)       = ",," ++ intercalate " " names
padded (GivenAndFamily gs fs) = intercalate " " gs ++ "," ++ intercalate " " fs

prepare :: Text -> [Text]
prepare = filter (not . T.null) . map (toTitle . strip) . splitOn " " . replace "," " "

