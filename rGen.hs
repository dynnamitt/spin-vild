import System.Random
import System.Exit
import Data.List
import System.Environment

type Color = Int -- TODO data type ??
data Tile = Sgl Color |
            Dbl Color
type TileWidth = Int

tileWidth :: Tile -> TileWidth
tileWidth (Sgl _) = 1
tileWidth (Dbl _) = 2

-- ctor            
newTile :: (TileWidth,Color) -> Tile
newTile (0,c) = Sgl c
newTile (_,c) = Dbl c


data Floor = Floor { 
    maxColors::Int,
    maxSizes::Int,
    breakAfter::Int 
    } deriving Show 

main = do
  
  floor <- parseArgs
  genA <- getStdGen
  genB <- newStdGen
  let colors = randInts genA $ maxColors floor
  let widths = randInts genB $ maxSizes floor
  -- TODO inject \n after x small-tiles 
  --   (1 big-tile-len == 2 small-tile-len )
  --  Buffer/Flush/State !! !
  let tiles = map newTile zip widths colors
  putStr $ concat.map (\ t -> showTileWithBreak t floor 0) $ zip widths colors
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
type Count = Int
showTileWithBreak :: Tile -> Floor -> Count -> (String,Count)
showTileWithBreak t f cnt =
  let tileW = tileWidth t
  in
    if cnt < breakAfter f 
    then 
      (showTile2 t, cnt - tileW)
    else
      (showTile2 t ++ "\n", 0)

showTile2 :: Tile -> String
showTile2 (Sgl c) = "|" ++ show c 
showTile2 (Dbl c) = "|" ++ show c ++ "  "

intsToLines :: [Int] -> String
intsToLines = concat . intersperse "\n" . map show 

randInts :: StdGen -> Int -> [Int]
randInts g maxInt = 
  let (n,g') = randomR (0,maxInt) g
  in n : randInts g' maxInt
