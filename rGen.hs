import System.Random

main = do
  -- getArgs ? Name , cash, escorts, time ...
  gen <- getStdGen
  putStr (show (randInts gen 36))
  -- FIXME \n infifine loop that will
  -- stream , intersperse w \n


randInts :: StdGen -> Int -> [Int]
randInts g maxInt = 
  let (n,g') = randomR (0,maxInt) g
  in n : randInts g' maxInt
