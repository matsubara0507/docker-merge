module Main where

import Options.Applicative

data Args = Args [FilePath] deriving (Show)

argsParser :: Parser Args
argsParser = Args <$> many (argument filepath (metavar "TARGET..."))

filepath :: ReadM FilePath
filepath = str

main :: IO ()
main = do
  Args fps <- execParser (info (argsParser <**> helper) idm)
  putStrLn $ unlines fps
