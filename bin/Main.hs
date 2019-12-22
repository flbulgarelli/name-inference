{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Options.Applicative
import Data.Semigroup ((<>))
import Data.Foldable (for_)
import Data.Text as T (Text, pack, unpack, splitOn, strip, replace, toTitle, null)
import Data.List (intercalate)
import Data.PersonalName

data Input = Input {
    givensFile :: String,
    familiesFile :: String,
    inputFile :: String,
    outputFormat :: String,
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
         <> help "givens filename" )
      <*> strOption (
        long "families"
         <> short 'f'
         <> metavar "FILE"
         <> help "families filename" )
      <*> strOption (
        long "file"
         <> short 'F'
         <> metavar "FILE"
         <> value "--"
         <> help "families filename" )
      <*> strOption (
        long "output-format"
         <> short 'o'
         <> metavar "tagged|csv|padded"
         <> value "tagged"
         <> help "output format. `tagged` by default" )
      <*> switch (
        long "transliterate"
         <> short 't'
         <> help "transliterate names" )
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
    opts = info (sample <**> helper) (fullDesc <> progDesc "Classify and flip personal names")

run :: Input -> IO ()
run (Input givens families file outputFormat transliterate unknownAsFamily breakFullNames) = do
  givens <- fmap lines (readFile givens)
  families <- fmap lines (readFile families)
  let registry = makeRegistry givens families (RegistryOptions transliterate unknownAsFamily)
  contents <- if file == "--" then getContents else readFile file
  for_ (lines contents) (processLine registry (selectFormat outputFormat) (selectDivider breakFullNames))

selectDivider :: Bool -> NameDivider
selectDivider False = splitNamesWith noBonus
selectDivider _     = justBreakNamesWith noBonus

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

