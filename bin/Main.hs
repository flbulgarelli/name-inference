module Main (main) where

import Options.Applicative
import Data.Semigroup ((<>))
import Data.Foldable (for_)
import Data.List.Split (splitOn)
import Data.PersonalName

data Input = Input {
    givensFile :: String,
    familiesFile :: String
  }

sample :: Parser Input
sample = Input
      <$> strOption (
        long "givens"
         <> metavar "FILE"
         <> help "givens filename" )
      <*> strOption (
        long "families"
         <> metavar "FILE"
         <> help "families filename" )


main :: IO ()
main = execParser opts >>= run
  where
    opts = info (sample <**> helper) (fullDesc <> progDesc "Classify and flip personal names")

run :: Input -> IO ()
run (Input givens families) = do
  givens <- fmap lines (readFile givens)
  families <- fmap lines (readFile families)
  let registry = makeRegistry givens families (defaultOptions { transliterateNames = True, treatUnknownAsFamily = True })
  contents <- getContents
  for_ (lines contents) (processLine registry)

processLine :: Registry -> String -> IO ()
processLine registry = putStrLn  . show . fix registry . FullName . splitOn " "


