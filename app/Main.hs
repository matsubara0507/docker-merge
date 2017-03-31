module Main where

import Data.Either (lefts, rights)
import Data.Maybe (fromMaybe)
import Options.Applicative
import Language.Dockerfile
import Language.Dockerfile.Syntax

data Args = Args [FilePath] deriving (Show)

argsParser :: Parser Args
argsParser = Args <$> many (argument filepath (metavar "TARGET..."))

filepath :: ReadM FilePath
filepath = str

toInstructions :: Dockerfile -> [Instruction]
toInstructions = fmap instruction

removeEOL :: [Instruction] -> [Instruction]
removeEOL = filter (/= EOL)

getFileName :: Dockerfile -> Maybe Filename
getFileName [] = Nothing
getFileName df = Just . sourcename . head $ df

mergeDockerfiles :: [Instruction] -> [Instruction] -> [Instruction]
mergeDockerfiles [] df = df
mergeDockerfiles xs@(x:xs') ys@(y:ys')
  | x == y = x : mergeDockerfiles xs' ys'
  | otherwise = xs `mappend` ys

printDockerfile :: Dockerfile -> String
printDockerfile dockerfile = unlines $ (filename `mappend` ":") : instructions
  where
    filename = fromMaybe "" . getFileName $ dockerfile
    instructions = fmap show . removeEOL . toInstructions $ dockerfile

main :: IO ()
main = do
  Args filepaths <- execParser (info (argsParser <**> helper) idm)
  dockerfiles <- mapM parseFile filepaths

  putStrLn "== can read =="
  putStrLn . unlines . fmap printDockerfile $ rights dockerfiles

  putStrLn "== merge =="
  putStrLn . unlines . fmap show . foldl mergeDockerfiles [] . fmap (removeEOL . toInstructions) $ rights dockerfiles

  putStrLn "== can't read =="
  print $ lefts dockerfiles
