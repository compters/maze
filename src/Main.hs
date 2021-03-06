module Main where

import Options.Applicative
import Maze
import Control.Applicative
import Data.Word
import Output

data MazeOptions = MazeOptions {
    rowCount      :: Int
  , colCount      :: Int
  , useSidewinder :: Bool
  , useWilsons    :: Bool
  , showDistances :: Bool
  , hideSolution  :: Bool
  , randomSeed    :: Maybe Word32
  , outputImg     :: Maybe String
  , imgWidth      :: Int
  , imgHeight     :: Int
  } deriving (Show, Eq)


runMazeIO :: MazeOptions -> IO ()
runMazeIO (MazeOptions rows cols sw ws dist hideS rSeed imgPath imgW imgH) = do
  maze <- newMaze (rows, cols) 0 rSeed
  (resMaze, stateMaze) <- runMaze (algo >> processor) maze
  if dist then
    ioAct stateMaze (colourCell stateMaze)
    else
      ioAct resMaze outputContents
    where
      processor = if hideS then blankCells else djikstra
      algo = if sw then sidewinder else if ws then wilsons else binaryTree
      ioAct :: GShow a => Maze a -> CellRenderer a ->  IO ()
      ioAct m r = case imgPath of
        Just p -> drawMazeWith r p imgW imgH m
        Nothing -> print m

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
                <*> switch (long "wilsons"
                            <> short 'w'
                            <> help "Use wilson's algorithm instead of binary tree")
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
                <*> optional (strOption
                             (long "output"
                             <> short 'o'
                             <> help "The filename to output to instead of ASCII"))
                <*> option auto (long "width"
                                <> value 400
                                <> help "Image width")
                <*> option auto (long "height"
                                <> value 400
                                <> help "Image height")
