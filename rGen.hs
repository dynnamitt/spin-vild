import System.Random
import Data.List
import System.Environment


main = do
  args <- getArgs 
  -- arg0=numColors , arg1=tileSizes, 
  -- arg2=maxTiles , arg3=wallLen
  let numColors = read $ args !! 0
  let tileSizes = read $ args !! 1 -- !! special logic
  let maxTiles = read $ args !! 2
  -- let wallLen = read $ args !! 3
  -- TODO maybe?either fix if no args given
  -- TODO usage print and exit
  genA <- getStdGen
  genB <- newStdGen
  let colors = randInts genA numColors
  let widths = randInts genB tileSizes
  -- TODO inject \n after x small-tiles 
  --   (1 big-tile-len == 2 small-tile-len )
  --  Buffer/Flush/State !! !
  putStr $ concat.map showTile $ take maxTiles $ zip widths colors
  -- putStr $ intsToLines $ randInts gen 3

type TileWidth = Int
type Color = Int
type Quads = Int -- Kvadrater i en Tile m'key?

data Tile = Small Color Quads |
            Big Color Quads

tile :: (TileWidth,Color) -> Tile
tile (0,c) = Small c 1
tile (_,c) = Big c 2

-- TODO
-- breakOnFullWallLength :: [Tile] -> (


-- always CAPPING size down to Either small or big
-- giving size0 low odds (pga high retail price :)
showTile :: (TileWidth,Color) -> String
showTile (0,c) = "|" ++ show c 
showTile (_,c) = "|" ++ show c ++ "  "

-- new version is
-- showTileAlongWall :: Tile -> Quads -> Quads -> String
-- showTileAlongWall tile qsLeft maxQs = () recurse

intsToLines :: [Int] -> String
intsToLines = concat . intersperse "\n" . map show 

randInts :: StdGen -> Int -> [Int]
randInts g maxInt = 
  let (n,g') = randomR (0,maxInt) g
  in n : randInts g' maxInt
