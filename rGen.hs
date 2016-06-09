import System.Random
import Data.List

main = do
  -- getArgs ? Name , cash, escorts, time ...
  genA <- getStdGen
  genB <- newStdGen
  putStr $ show $ take 100 $ zip ( randInts genA 2 ) ( randInts genB 1 )
  -- putStr $ intsToLines $ randInts gen 3

joinedInts :: (Int,Int) -> String
joinedInts (a,b) = show a ++ " " ++ show b

intsToLines :: [Int] -> [Char]
intsToLines = concat . intersperse "\n" . map show 

randInts :: StdGen -> Int -> [Int]
randInts g maxInt = 
  let (n,g') = randomR (0,maxInt) g
  in n : randInts g' maxInt
