module Main where

import Options.Applicative
import Maze


data MazeOptions = MazeOptions {
    rowCount :: Int
  , colCount :: Int
 } deriving (Show, Eq)


runMazeIO :: MazeOptions -> IO ()
runMazeIO (MazeOptions rows cols) = maze >>= print
    where
      maze = newMaze (rows, cols) 0 >>= evalMaze (binaryTree >> djikstra)

main :: IO ()
main = execParser opts >>= runMazeIO
 where
   opts = info (helper <*> optionsParser) (fullDesc <> progDesc "Output a maze")

optionsParser :: Parser MazeOptions
optionsParser = MazeOptions
                <$> option auto (long "rows"
                              <> value 8 
                              <> short 'r'
                              <> help "How many rows to output")
                <*> option auto (long "columns"
                              <> value 8 
                              <> short 'c'
                              <> help "How many columns to output")
