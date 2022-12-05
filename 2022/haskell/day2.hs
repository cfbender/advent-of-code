import Data.Char
import Data.List
import Data.List.Split
import System.Environment
import System.IO

charToNum :: Char -> Int
-- gets the ascii value of the character, mod 23 to bring ABC and XYZ equal, then mod 3 to see which one it is in order
charToNum = succ . (`mod` 3) . (`mod` 23) . pred . ord

parse :: String -> [(Int, Int)]
parse = map ((\[a, b] -> (charToNum $ head a, charToNum $ head b)) . words) . lines

main = do
  args <- getArgs
  content <- readFile "inputs/input2_test.txt"
  print $ "Part 1: " ++ show (parse content)
