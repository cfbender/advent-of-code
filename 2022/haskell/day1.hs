import Data.List
import Data.List.Split

import System.IO
import System.Environment

parse :: String -> [[Int]]
parse = map (map read) . splitWhen null . lines

main = do
   args <- getArgs
   content <- readFile "input.txt"
   print $ "Part 1: " ++ show(maximum $ map sum $ parse content)
   print $ "Part 2: " ++ show(sum $ take 3 $ reverse . sort $ map sum $ parse content)
