import System.Random
import Data.List

main = do
  -- getArgs ? Name , cash, escorts, time ...
  gen <- getStdGen
  putStr $ concat . intersperse "\n" $ map show (randInts gen 36)


randInts :: StdGen -> Int -> [Int]
randInts g maxInt = 
  let (n,g') = randomR (0,maxInt) g
  in n : randInts g' maxInt
