module Main where

import Data.Either (lefts, rights)
import Options.Applicative
import Language.Dockerfile
import Language.Dockerfile.Syntax

data Args = Args [FilePath] deriving (Show)

argsParser :: Parser Args
argsParser = Args <$> many (argument filepath (metavar "TARGET..."))

filepath :: ReadM FilePath
filepath = str

printDockerfile :: Dockerfile -> String
printDockerfile dockerfile = unlines $ (filename `mappend` ":") : instructions
  where
    filename = sourcename $ head dockerfile
    instructions = fmap (show .instruction) dockerfile

main :: IO ()
main = do
  Args filepaths <- execParser (info (argsParser <**> helper) idm)
  dockerfiles <- mapM parseFile filepaths

  putStrLn "== can read =="
  putStrLn . unlines . fmap printDockerfile $ rights dockerfiles

  putStrLn "== can't read =="
  print $ lefts dockerfiles
