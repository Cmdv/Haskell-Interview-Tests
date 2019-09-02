{-# LANGUAGE ScopedTypeVariables #-}

module SpiralWorld where

import           Control.Applicative  hiding (many, some)
import           Control.Arrow        ((&&&))
import           Control.Monad
import           Data.Foldable
import qualified Data.Map             as M
import           Data.Maybe
import           Data.Traversable
import           Data.Tuple
import qualified Data.UnionFind.IO    as U
import           Data.Void
import           Text.Megaparsec      hiding (State)
import           Text.Megaparsec.Char


-- | The Parser for parsing the original content of the scroll
type Parser = Parsec Void String

scrollParser :: Parser [Int]
scrollParser = do
  let d = '~'
      h = '#'
  start  <- try $ char d <|> char h
  tildes <- many $ try (char d) <|> char d <* eof
  mEnd   <- optional (char h)

  let lgthTildes = length tildes
      end = catMaybes [mEnd]
      -- should the population count the # or not
      nPop = if start == h || null end then lgthTildes else lgthTildes + 1
      -- make sure to add a tilde to be replicated if it's caught by start
      nTildes = if start == d && null end then lgthTildes else lgthTildes + 1
  -- check for ## as that results in 00
  if start == h && end == [h] && null tildes
    then return [0,0]
    -- replicate the number of tildes as 0's
    else return $
      replicate nTildes 0
      <> (if nPop == 0 && null tildes then [0] else [mod nPop 10])
      -- ^ append the number of population


-- | Generating and combining the parsed scroll into an outward spiral using (x,y)
-- | of cartisian grid.

type Coordinate = (Int, Int)

-- | Cycles a series of functions into an outwardly expanding spiral pattern
movePattern :: [a -> a] -> [a -> a]
movePattern funcs = go ns dirs
  -- generate the spiral move pattern [1,1,2,2,3,3,4,4...]
  where ns = replicate 2 =<< [1..]
        dirs = cycle funcs
        -- combined with ns this generates [mvRight,mvDown,mvLeft,mvLeft...]
        go (x:xs) (f:fs) = replicate x f ++ go xs fs

-- | List corresponding to each sequential number's coordinate location
mkCoordList :: Coordinate -> [Coordinate]
mkCoordList seed = seed : go moveList seed
  where go (f:fs) coord = f coord : go fs (f coord)
        moveList = movePattern [mvRight, mvDown, mvLeft, mvUp]
        mvUp    (x, y) = (x, y+1)
        mvDown  (x, y) = (x, y-1)
        mvLeft  (x, y) = (x-1, y)
        mvRight (x, y) = (x+1, y)

-- |
mkCordsSpiralMtx :: [Int] ->  M.Map Coordinate Int
mkCordsSpiralMtx parsedScroll = M.fromList $ zip (mkCoordList (0,0)) parsedScroll


-- | Take Map of land coordinates and return list of grouped land items forming islands
-- | Using Union find algorythm
findIslands ::  M.Map Coordinate Coordinate -> IO [[Coordinate]]
findIslands land = do
  -- create fresh point map
  pointMap <- traverse U.fresh land
  -- traverse each point checjing for neighbours
  void . flip M.traverseWithKey pointMap $ \(x, y) point ->
      for_ (catMaybes (flip M.lookup pointMap <$> [(x + 1, y), (x, y + 1),(x +1, y +1), (x - 1, y + 1)]))
          $ \neighbourPoint ->
              U.union point neighbourPoint
  -- traverse ppintMap and representative and their descriptors
  withUnionKey :: (M.Map Coordinate Coordinate) <- for pointMap (U.repr >=> U.descriptor)
  -- swap cordinates arround
  let unionKeyToCoord :: [(Coordinate, Coordinate)] = (swap <$> M.toList withUnionKey)
      -- combine coordinates to create islands
      results :: M.Map Coordinate [Coordinate] = M.fromListWith (<>) (fmap (:[]) <$> unionKeyToCoord)
  -- return just the elements from the Map
  return (M.elems results)

convertTolandGrid :: [Coordinate] -> M.Map Coordinate Coordinate
convertTolandGrid = M.fromList . fmap (id &&& id)


-- | The main functions to get the app started
spiralWorld :: FilePath -> IO ()
spiralWorld fp = do
  txt <- readFile fp
  -- parse the file and return [[Int]]
  case parse (many scrollParser) fp txt of
    Left err   -> putStr (errorBundlePretty err)
    Right expr -> do
          -- add spiral coordinate to parsed expr
      let matrixScroll :: M.Map Coordinate Int     = mkCordsSpiralMtx $ concat expr
          -- filter out all the sea items
          landMatrixScroll :: M.Map Coordinate Int = M.filter (/= 0) matrixScroll
          -- keys of all the land items
          landMSKeys :: [Coordinate]               = M.keys landMatrixScroll
      -- using the land keys combine all nearest items into islands
      islands :: [[Coordinate]] <- findIslands $ convertTolandGrid landMSKeys

      let popIslands :: [[Int]] = getPopulation islands landMatrixScroll
          biggestIslandPop = maximum $ map sum popIslands
      putStrLn ("biggest Island population is: " <> show biggestIslandPop)

-- | Do lookup and get population for each item in island
getPopulation :: [[Coordinate]] -> M.Map Coordinate Int -> [[Int]]
getPopulation islands lms =
  fmap (\a -> M.findWithDefault 0 a lms) <$> islands


main :: IO ()
main = do
     args <- getArgs
     case args of
      [f] -> spiralWorld f
      _   -> putStrLn "Incorrect/missing File path"
