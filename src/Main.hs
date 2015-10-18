module Main where

import Options.Applicative
import Maze
import Control.Applicative
import Data.Word

data MazeOptions = MazeOptions {
    rowCount      :: Int
  , colCount      :: Int
  , useSidewinder :: Bool
  , showDistances :: Bool
  , hideSolution  :: Bool
  , randomSeed    :: Maybe Word32
 } deriving (Show, Eq)


runMazeIO :: MazeOptions -> IO ()
runMazeIO (MazeOptions rows cols sw dist hideS rSeed) = do
  maze <- newMaze (rows, cols) 0 rSeed
  (resMaze, stateMaze) <- runMaze (algo >> processor) maze
  if dist then
    print stateMaze
    else
      print resMaze
    where
      processor = if hideS then blankCells else djikstra
      algo = if sw then sidewinder else binaryTree

main :: IO ()
main = execParser opts >>= runMazeIO
 where
   opts = info (helper <*> optionsParser) (fullDesc <> progDesc "Output a maze")

optionsParser :: Parser MazeOptions
optionsParser = MazeOptions
                <$> option auto (long "rows"
                              <> value 10
                              <> short 'r'
                              <> help "How many rows to output")
                <*> option auto (long "columns"
                              <> value 10
                              <> short 'c'
                              <> help "How many columns to output")
                <*> switch (long "sidewinder"
                            <> short 's'
                            <> help "Use sidewinder instead of binary tree")
                <*> switch (long "distances"
                            <> short 'd'
                            <> help "Display distances to nodes instead of solution")
                <*> switch (long "hide"
                            <> short 'z'
                            <> help "Hides the solution from being shown")
                <*> optional (option auto
                              (long "random"
                               <> short 'x'
                               <> help "The seed for the random number generator"))
