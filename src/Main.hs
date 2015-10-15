module Main where

import Debug.Trace
import Data.List
import Data.Vector (Vector)
import qualified Data.Vector as V
import System.Random.MWC
import Data.Map (Map)
import qualified Data.Map as Map

data C = Wall
       | Cl Cell
  deriving (Eq, Ord)

instance Show C where
  show Wall   = "Wall"
  show (Cl _) = "Cell"

data Cell = Cell { row :: !Int, col :: !Int, links :: Map Cell Bool, north :: C, south :: C, east :: C, west :: C  }
    deriving (Ord)

instance Show Cell where
  show (Cell r c _ n s e w) = "|C(" ++ show r ++ "," ++ show c ++ ") " ++ "N-" ++ show n ++ ",S-" ++ show s ++ ",E-" ++ show e ++ ",W-" ++ show w ++ "|"

type Row = Vector Cell

data Grid = Grid { rows :: !Int, cols :: !Int, grid :: Vector Row }
    deriving (Eq)

renderRow :: Row -> String
renderRow cells = let (top, bottom) = foldl' toS ("", "") cells in
  "|" ++ top ++ "\n" ++ "+" ++ bottom
instance Show Grid where
  show (Grid r c g) = prelude ++ "\n" ++ body
      where
        prelude = "+" ++ (concat $ replicate c "---+")
        body = intercalate "\n" (V.toList $ V.map renderRow g)
toS (top, bottom) c@(Cell _ _ _ _ s e _) = (top ++ "   " ++ boundary c e, bottom ++ sBoundary c s ++ "+")
sBoundary c Wall = "---"
-- The C' values here are the OLD values of c', pre links!
sBoundary c (Cl c') = if linked c' c then "   " else "---"
boundary c Wall = "|"
boundary c (Cl c') = if linked c' c then " " else "|"

instance Eq Cell where
  (==) c c' = (row c == row c') && (col c == col c')

newCell :: Int -> Int -> Grid -> Cell
newCell r c grid = Cell r c Map.empty (get grid n) (get grid s) (get grid e) (get grid w)
    where
      n = (r - 1, c)
      s = (r + 1, c)
      e = (r, c + 1)
      w = (r, c - 1)

get :: Grid -> (Int, Int) -> C
get (Grid rows cols g ) a@(r, c) = if inBounds then Cl ((g V.! r) V.! c) else Wall
  where inBounds = r >= 0 && r < rows && c >= 0 && c < cols

newGrid :: Int -> Int -> Grid
newGrid r c = gg
    where g = V.fromList [newRow i | i <- [0 .. r -1]]
          newRow i = V.fromList [newCell i j gg | j <- [0..c-1]]
          gg = Grid r c g

linkCells :: Grid -> Cell -> Cell -> Grid
linkCells g a b = replace b' $ replace a' g
    where
      a' = a { links = Map.insert b True (links a) }
      b' = b { links = Map.insert a True (links b) }

unlinkCells :: Cell -> Cell -> (Cell, Cell)
unlinkCells a b = (a', b')
    where
      a' = a { links = Map.delete b (links a) }
      b' = b { links = Map.delete a (links b) }

linked :: Cell -> Cell -> Bool
linked a b = Map.member a (links b)

neighbors :: Cell -> [Cell]
neighbors (Cell _ _ _ n s e w) = [c | Cl c <- [n, s, e, w]]

eachCell :: Monad m => Grid -> (Grid -> Cell -> m Grid) -> m Grid
eachCell g@(Grid r c rows) f = V.foldM' foldRow g rows
  where
    foldRow = V.foldM' f

replace :: Cell -> Grid -> Grid
replace c@(Cell row col _ _ _ _ _) g = g { grid = (grid g) V.// [(row, gRow)] }
   where gRow = ((grid g) V.! row) V.// [(col, c)]

binaryTree :: Int -> Int -> IO Grid
binaryTree rows cols = do
  let grid = newGrid rows cols
  rand <- createSystemRandom
  g' <- eachCell grid (walker rand)
  return g'
  where
    walker :: GenIO -> Grid -> Cell -> IO Grid
    walker rand g c@(Cell row col links n _ e _) = do
      let candidates = [cx | Cl cx <- [n, e]]
      if not (null candidates) then do
        i <- uniformR (0, length candidates - 1) rand
        return $ linkCells g (candidates !! i) c
      else return g

main :: IO ()
main = do
  grid <- binaryTree 10 10
  print grid
