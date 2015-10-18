{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving, RankNTypes, OverlappingInstances, FlexibleInstances #-}

module Maze where

import Debug.Trace
import Data.Word
import Control.Monad
import Control.Lens
import Control.Monad.State.Strict
import Data.List
import Data.Vector (Vector, (!), (//))
import qualified Data.Vector as V
import Data.Set (Set)
import qualified Data.Set as Set
import System.Random.MWC

class (Show a) => GShow a where
  gshow :: a -> String
  gshow = show

instance GShow String where
  gshow = id

instance GShow Char where
  gshow c = [c]

instance GShow Int where
  gshow = show

data C = Wall
       | Cl (Int, Int)
    deriving (Eq, Ord, Show)

data Cell a = Cell {
        _row   :: !Int
      , _col   :: !Int
      , _links :: Set (Int, Int)
      , _north :: C
      , _south :: C
      , _east  :: C
      , _west  :: C
      , _val   :: a
  }
  deriving (Show, Eq)
makeLenses ''Cell

type Row a = Vector (Cell a)

data Maze a = Maze {
    _rowCount  :: !Int
  , _colCount  :: !Int
  , _grid      :: Vector (Row a)
  , _rand      :: GenIO
  }

makeLenses ''Maze

instance Functor Maze where
  fmap f m = m & grid %~ newGrid
      where
        newGrid = V.map (mapRow f)
        mapCell f c = c & val %~ f
        mapRow f = V.map (mapCell f) 

instance GShow a => Show (Maze a) where
  show maze = header ++ body
      where
        header = "+" ++ concat (replicate (maze^.colCount) cellSection) ++ "\n"
        body = intercalate "\n" (V.toList $ V.map renderRow (maze^.grid))

type MZ x = StateT (Maze x)

newtype MazeM x a = MazeM { eval :: MZ x IO a }
    deriving (Functor, Applicative, Monad, MonadState (Maze x), MonadIO)

cellWidth = 3
topWall = concat $ replicate cellWidth "-"
cellGap = concat $ replicate cellWidth " "
cellSection = topWall ++ "+"

update :: Cell x -> MazeM x ()
update c = grid %= replaceRow
  where
    replaceRow g = g // [(c^.row, replaceCell (g ! (c^.row)))]
    replaceCell r = r // [(c^.col, c)]

eachCell :: r -> (r -> Cell a -> MazeM a r) -> MazeM a r
eachCell v fn = do
  rows <- use rowCount
  cols <- use colCount
  foldM runFn v [(r, c) | r <- [0 .. rows - 1], c <- [0 .. cols - 1]]
  where
    runFn v' crds = getCell crds >>= fn v'

eachCell_ :: (Cell a -> MazeM a ()) -> MazeM a ()
eachCell_ fn = eachCell () (\ _ c -> fn c)

coords :: Cell a -> (Int, Int)
coords c = (c^.row, c^.col)

getCell :: (Int, Int) -> MazeM a (Cell a)
getCell (r, c) = do
  g <- use grid
  return $ (g ! r) ! c

linkCells :: Cell a -> Cell a -> MazeM a ()
linkCells a b = do
  update a'
  update b'
  where
    a' = a & links %~ Set.insert (coords b)
    b' = b & links %~ Set.insert (coords a)

unlinkCells :: Cell a -> Cell a -> MazeM a ()
unlinkCells a b = do
  update a'
  update b'
  where
    a' = a & links %~ Set.delete (coords b)
    b' = b & links %~ Set.delete (coords a)

neighbors :: Cell a -> MazeM a [Cell a]
neighbors c = sequence [getCell cl | Cl cl <- [n, s, e, w]]
    where
      n = c^.north
      s = c^.south
      e = c^.east
      w = c^.west

coinToss :: MazeM a Bool
coinToss = do
  r <- use rand
  liftIO $ uniform r

pickRandomly :: [x] -> MazeM a x
pickRandomly xs = do
  r <- use rand
  i <- liftIO $ uniformR (0, length xs - 1) r
  return $ xs !! i

pickRandomlyM :: [x] -> MazeM a (Maybe x)
pickRandomlyM [] = return Nothing
pickRandomlyM xs = Just <$> pickRandomly xs

newMaze :: (Int, Int) -> a -> Maybe Word32 -> IO (Maze a)
newMaze (rows, cols) v rSeed = do
  rnd <- rndGen
  return $ Maze rows cols newGrid rnd
  where
    rndGen = case rSeed of
      Just seed -> initialize (V.singleton seed)
      Nothing -> createSystemRandom
    newGrid     = V.fromList [newRow r | r <- [0 .. rows - 1]]
    newRow r = V.fromList [newCell (r, c) | c <- [0 .. cols - 1]]
    newCell (r, c) = Cell r c Set.empty n s e w v 
      where
        n = getC (r - 1, c)
        s = getC (r + 1, c)
        e = getC (r, c + 1)
        w = getC (r, c - 1)
    getC (r, c) =
      if inBounds then Cl (r, c) else Wall
      where
        inBounds = r >= 0 && r < rows && c >= 0 && c < cols

linked :: Cell a -> (Int, Int) -> Bool
linked cell crds = Set.member crds (cell^.links) 

renderRow :: GShow a => Row a -> String
renderRow cells = let (rowTop, rowBottom) = foldl renderCell ("", "") cells in
  "|" ++ rowTop ++ "\n" ++ "+" ++ rowBottom

padContents :: GShow a => a -> String
padContents x = pad (gshow x)
    where
      pad str | length str < cellWidth && length str `mod` 2 == 0 = pad $ str ++ " "
              | length str < cellWidth && length str `mod` 2 /= 0 = pad $ " " ++ str
              | otherwise = str

renderCell :: GShow a => (String, String) -> Cell a -> (String, String)
renderCell (rowTop, rowBottom) cell = (rowTop', rowBottom')
    where
      rowTop'    = rowTop ++ padContents (cell^.val) ++  eastBoundary
      rowBottom' = rowBottom ++ southBoundary ++ "+"
      eastBoundary = case cell^.east of
                       Wall -> "|"
                       Cl c -> if linked cell c then " " else "|"
      southBoundary = case cell^.south of
                       Wall -> topWall
                       Cl c -> if linked cell c then cellGap else topWall

binaryTree :: MazeM a ()
binaryTree = eachCell_ walker
    where
      walker cell = do
        let candidates = [c | Cl c <- [cell^.north, cell^.east]]
        unless (null candidates) $ do
          cell' <- pickRandomly candidates >>= getCell
          linkCells cell cell'

linkCl :: Cell a -> C -> MazeM a ()
linkCl _ Wall = return ()
linkCl cell (Cl c) = do
  cl <- getCell c
  linkCells cell cl

sidewinder :: MazeM a ()
sidewinder = void $ eachCell ([], 1) winder
    where
      atWall Wall = True
      atWall _ = False

      linkNorth (Just cl) = do
        cell <- getCell cl
        linkCl cell (cell^.north)
      linkNorth Nothing = return ()

      winder (run, rrow) cell = do
        let run' = if rrow == (cell^.row) then (coords cell) : run else [coords cell]
        let atEasternBoundry = atWall $ cell^.east
        let atNorthernBoundary = atWall $ cell^.north
        coinResult <- coinToss
        let closeOut = atEasternBoundry || (not atNorthernBoundary && coinResult)
        if closeOut then
          do
            candidate <- pickRandomlyM run'
            linkNorth candidate
            return ([], cell^.row)
          else do
            linkCl cell (cell^.east)
            return (run', cell^.row)

execMaze :: MazeM a b -> Maze a -> IO (Maze a)
execMaze f = execStateT (eval f)

evalMaze :: MazeM a b -> Maze a -> IO b
evalMaze f = evalStateT (eval f)

runMaze :: MazeM a b -> Maze a -> IO (b, Maze a)
runMaze f = runStateT (eval f)

uniformM :: (Int, Int) -> MazeM a Int
uniformM x = do
  r <- use rand
  liftIO $ uniformR x r

randomWestWallCoord :: MazeM a (Int, Int)
randomWestWallCoord = do
  rows <- use rowCount
  row <- uniformM (0, rows - 1)
  return (row, 0)

randomCoord :: MazeM a (Int, Int)
randomCoord = do
  rows <- use rowCount
  cols <- use colCount
  row <- uniformM (0, rows - 1)
  col <- uniformM (0, cols - 1)
  return (row, col)

distanceFill :: (Int, Int) -> MazeM Int (Int, (Int, Int))
distanceFill start = do
  startCell <- getCell start
  update $ startCell & val .~ 0
  walkLinks 0 (Set.toList $ startCell^.links) (0, start)
    where
      walkLinks _ [] biggest = return biggest
      walkLinks c cells (n, bg) = do
        frontier <- mapM (updateCell (c + 1)) cells
        walkLinks (c + 1) (concat frontier) (c + 1, head cells)
      updateCell c crds = do
        cell <- getCell crds
        update $ cell & val .~ c
        filterM isZero $ Set.toList $ cell^.links
      isZero c = do
        cell <- getCell c
        return $ cell^.val == 0 && c /= start

distanceFillRand :: MazeM Int (Int, (Int, Int))
distanceFillRand = randomWestWallCoord >>= distanceFill

mapCells :: (Cell a -> b) -> Maze a -> Maze b
mapCells fn m = m & grid %~ newGrid
    where
      newGrid   = V.map updateRow
      updateRow = V.map updateCellValue
      updateCellValue cell = cell { _val = fn cell }

blankCells :: MazeM a (Maze Char)
blankCells = do
  mz <- get
  return $ mapCells (const ' ') mz

djikstra :: MazeM Int (Maze Char)
djikstra = do
  path <- distanceFillAndWalk
  mz <- get
  return $ mapCells (markPath path) mz
  where
        distanceFillAndWalk = do
          (_, end) <- distanceFillRand
          endCell <- getCell end
          walkLinks endCell [end]
        walkLinks cell acc =
          if cell^.val == 0 then
            return $ Set.fromList acc
          else do
            (next, _) <- smallestLink cell
            walkLinks next (coords next : acc)
        pairValue :: (Int, Int) -> MazeM Int (Cell Int, Int)
        pairValue coord = do
          cell <- getCell coord
          return (cell, cell^.val)
        smallestLink :: Cell a -> MazeM Int (Cell Int, Int)
        smallestLink cell = do
          linkedCells <- mapM pairValue $ Set.toList (cell^.links)
          return $ minimumBy pairCompare linkedCells
        pairCompare (_, b) (_, b') = compare b b'
        markPath path cell = if Set.member (coords cell) path then '*' else ' '
