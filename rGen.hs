import System.Random
import System.Exit
import Data.List
import System.Environment


data Quads = One | Two -- Kvadrater i en Tile m'key?

data Tile = Small Color Quads |
            Big Color Quads

type TileWidth = Int
type Color = Int -- TODO data type ??

-- ctor            
newTile :: (TileWidth,Color) -> Tile
newTile (0,c) = Small c One
newTile (_,c) = Big c Two


data Floor = Floor { 
    maxColors::Int,
    maxSizes::Int,
    breakAfter::Int 
    } deriving Show 

main = do
  
  pArgs <- parseArgs
  genA <- getStdGen
  genB <- newStdGen
  let colors = randInts genA $ maxColors pArgs
  let widths = randInts genB $ maxSizes pArgs
  -- TODO inject \n after x small-tiles 
  --   (1 big-tile-len == 2 small-tile-len )
  --  Buffer/Flush/State !! !
  putStr $ concat.map showTile $ zip widths colors
  -- putStr $ intsToLines $ randInts gen 3
  --
parseArgs :: IO Floor
parseArgs = do
  args <- getArgs
  if length args < 3 
    then do
      usage
      exitWith $ ExitFailure 1
    else do 
      -- TODO errorhandling
      let maxColors = read $ args !! 0 :: Int
      let maxSizes = read $ args !! 1 :: Int
      let breakAfter = read $ args !! 1 :: Int
      return $ Floor maxColors maxSizes breakAfter

usage :: IO ()
usage = do
  prog <- getProgName
  putStrLn "usage:"
  putStrLn $ "  " ++ prog ++ " max-colors max-sizes break-after"
  -- hmm difficult to exit here

-- always CAPPING size down to Either small or big
-- giving size0 low odds (pga high retail price :)
showTile :: (TileWidth,Color) -> String
showTile (0,c) = "|" ++ show c 
showTile (_,c) = "|" ++ show c ++ "  "

-- new version 
type State = Int
showTileWithBreak :: Tile -> Floor -> State -> String
showTileWithBreak t f s = ""



intsToLines :: [Int] -> String
intsToLines = concat . intersperse "\n" . map show 

randInts :: StdGen -> Int -> [Int]
randInts g maxInt = 
  let (n,g') = randomR (0,maxInt) g
  in n : randInts g' maxInt
