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

merge :: [Dockerfile] -> Dockerfile
merge = toDockerfile . foldl mergeDockerfiles [] . fromDockerfile
  where
    toDockerfile = fmap instructionPos
    fromDockerfile = fmap (removeEOL . toInstructions)

main :: IO ()
main = do
  Args filepaths <- execParser (info (argsParser <**> helper) idm)
  dockerfiles <- mapM parseFile filepaths

  case lefts dockerfiles of
    [] -> putStrLn . prettyPrint . merge $ rights dockerfiles
    es -> putStrLn $ "error: " `mappend` show es
