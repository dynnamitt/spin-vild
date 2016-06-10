import System.Random
import System.Exit
import Data.List
import System.Environment

type TileWidth = Int
type Color = Int
type Quads = Int -- Kvadrater i en Tile m'key?

data Tile = Small Color Quads |
            Big Color Quads

main = do
  
  pArgs <- parseArgs
  genA <- getStdGen
  genB <- newStdGen
  let colors = randInts genA $ fst pArgs
  let widths = randInts genB $ snd pArgs
  -- TODO inject \n after x small-tiles 
  --   (1 big-tile-len == 2 small-tile-len )
  --  Buffer/Flush/State !! !
  putStr $ concat.map showTile $ zip widths colors
  -- putStr $ intsToLines $ randInts gen 3
  --
parseArgs :: IO (Int,Int)
parseArgs = do
  args <- getArgs
  if length args < 2 
    then do
      usage
      exitWith $ ExitFailure 1
    else do 
      -- TODO errorhandling
      let maxColors = read $ args !! 0 :: Int
      let maxSizes = read $ args !! 1 :: Int
      return (maxColors , maxSizes)

usage :: IO ()
usage = do
  prog <- getProgName
  putStrLn "usage:"
  putStrLn $ "  " ++ prog ++ " max-colors max-sizes"

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
