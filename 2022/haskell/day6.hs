import Data.List

findMarker :: String -> Int -> Int -> Int
findMarker message count size
  | size == length window = count + size
  | otherwise = findMarker (drop 1 message) (count + 1) size
  where
    window = nub $ take size message

main = do
  content <- readFile "inputs/input6.txt"
  print $ "Part 1: " ++ show (findMarker content 0 4)
  print $ "Part 2: " ++ show (findMarker content 0 14)
