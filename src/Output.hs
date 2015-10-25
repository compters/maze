{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts #-}
module Output where

import Maze
import Diagrams.Prelude
import Diagrams.Backend.Cairo
-- import Control.Lens hiding ((#), transform)
import qualified Data.Vector as V

type CellRenderer a = Cell a -> Diagram B -> Diagram B

renderMazeWith :: CellRenderer a -> Maze a -> Diagram B
renderMazeWith cellFn mz = (allRows # translateX (-0.5 * (cols - 1)))
                                    # translateY ( 0.5 * (rows - 1))
                         <> surroundingBox
  where
    surroundingBox :: Diagram B
    surroundingBox  = rect cols rows # fc white 
    allRows         = vsep 0 $ map outputRow $ V.toList (mz^.grid)
    outputRow r     = hsep 0 $ map outputCell $ V.toList r
    outputCell cell = cellFn cell (drawCell cell)
    rows            = fromIntegral $ mz^.rowCount
    cols            = fromIntegral $ mz^.colCount

drawCell :: Cell a -> Diagram B
drawCell cell = square 1
                 # explodeTrail -- Now seperate lines
                 # zipWith opacity wallOpacity
                 # mconcat 
                 # pad 1
   where
     calcOpacity dir = case cell^.dir of
                      Wall -> 1.0
                      Cl c -> if linked cell c then 0 else 1.0 
     wallOpacity = map calcOpacity [east, north, west, south]

outputContents :: GShow a => CellRenderer a 
outputContents cell diagram = contents <> diagram
    where contents = text (gshow $ cell^.val)
                     # fontSizeL 0.2
                     # fc black

colourCell :: (Integral a) => Maze a -> CellRenderer a -- Cell a -> Diagram B -> Diagram B
colourCell maze cell diagram = diagram <>
                                       square 1
                                     # lw none
                                     # fc cellColour
   where
     maxVal = fromIntegral $ maximum $ V.toList $ V.map (^.val) (allCells' maze)
     cellValue = toRational (cell^.val)
     intensity = (maxVal - cellValue) / maxVal
     dark = round $ 255.0 * intensity
     bright = round $ 128.0 + (127.0 * intensity)
     cellColour = sRGB24 bright dark dark

test :: IO ()
test = do
  m <- newMaze (20, 20) 0 (Just 11)
  mz <- execMaze (wilsons >> djikstra) m
  drawMazeWith (colourCell mz) "test.png" 800 800 mz
  print mz


drawMazeWith :: CellRenderer a -> FilePath -> Int -> Int -> Maze a -> IO ()
drawMazeWith cellRenderer filePath width height maze = renderCairo filePath size renderedMaze
  where
    size = mkSizeSpec2D (Just $ fromIntegral width) (Just $ fromIntegral height)
    renderedMaze = renderMazeWith cellRenderer maze

drawMaze :: FilePath -> Int -> Int -> Maze a -> IO ()
drawMaze = drawMazeWith (\ _ d -> d) 
