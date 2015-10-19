{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts #-}
module Output where

import Maze
import Diagrams.Prelude
import Diagrams.Backend.Cairo
import Diagrams.TwoD.Size
import Control.Lens hiding ((#))
import qualified Data.Vector as V

d :: Diagram B
d = circle 1

renderMaze :: Maze a -> Diagram B
renderMaze mz = mconcat [boundingBox, lineThing, lineThing]
  where
    boundingBox = fromOffsets [(-rows) *^ unitY, cols *^ unitX,
                               rows *^ unitY, (-cols) *^ unitX]
    allRows = V.map
    rows = fromIntegral $ mz^.rowCount
    cols = fromIntegral $ mz^.colCount

drawCell :: GShow a => Cell a -> Diagram B
drawCell cell = fromOffsets []
   where
     contents = text (gshow $ cell^.val) # fontSizeL 0.2 # fc black <> circle 0.2 # fc green
     eastBoundary = case cell^.east of
                     Wall -> unitY
                     Cl c -> if linked cell c then mempty else unitY
     southBoundary = case cell^.south of
                     Wall -> unitX
                     Cl c -> if linked cell c then mempty else unitX

test :: IO ()
test = do
  m <- newMaze (10, 10) 0 (Just 10)
  mz <- evalMaze (sidewinder >> djikstra) m
  drawMaze "test.png" 400 400 mz

drawMaze :: FilePath -> Int -> Int -> Maze a -> IO ()
drawMaze fp w h maze = renderCairo fp sz (renderMaze maze)
  where sz = mkSizeSpec2D (Just $ fromIntegral w) (Just $ fromIntegral h)
