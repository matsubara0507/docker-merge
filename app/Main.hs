module Main where

import Options.Applicative
import Language.Dockerfile

data Args = Args [FilePath] deriving (Show)

argsParser :: Parser Args
argsParser = Args <$> many (argument filepath (metavar "TARGET..."))

filepath :: ReadM FilePath
filepath = str

main :: IO ()
main = do
  Args filepaths <- execParser (info (argsParser <**> helper) idm)
  dockerfiles <- mapM parseFile filepaths
  print dockerfiles
