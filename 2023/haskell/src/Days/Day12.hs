module Days.Day12 where

-- sheesh this one was hard.
--
-- I REALLY liked how I did part 1,
-- with the smarter brute force and using replicateM to make the binary numbers
-- I thought it was really cool. but then part 2. man.
--
--
-- I really wish I knew dynamic programming more,
-- I've only used it like once and it was 2D. 2D is much easier
-- to think through, but 3D is tough. I think this is effectively the 2D
-- solution though where each one on the second dimension checks the groups
-- forward rather than iterating through them
--
-- I really like the array in haskell can be set to index by anything
-- and not just integers, though I suppose that's no different than a hashmap
--
-- Time to seriously learn some DP, though I feel like I learned a lot here with memoization
-- even though I kinda just copied someone's solution I found.
--
-- onto day 13!

{- ORMOLU_DISABLE -}
import Control.Monad
import Data.List
import qualified Data.List  as List
import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text (Parser, sepBy, endOfLine, char, many1, choice, space, decimal)
import qualified Data.Array as Array
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser part1 part2

------------ PARSER ------------
inputParser :: Parser Input
inputParser = springLine `sepBy` endOfLine
  where
    springLine = do
      springs <- many1 (choice [char '#', char '.', char '?'])
      space
      counts <- decimal `sepBy` char ','
      return (springs, counts)

------------ TYPES ------------

type Input = [(String, [Int])]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
-- springs is binary rep of filled springs list
-- groups is input (eg. 1,1,3)
check :: [Int] -> [Int] -> Bool
check groups springs = groups == sGroups
  where
    sGroups =
      map sum
        . filter (/= [0])
        $ groupBy (\a b -> a == 1 && b == 1) springs

-- decimal to binary list of 1 or 0
binary :: Int -> [Int]
binary 0 = [0]
binary n = go n
  where
    go 0 = []
    go k = mod k 2 : go (div k 2)

-- n is number of ?s
-- f is number missing
combos :: Int -> Int -> [[Int]]
combos n f = filter ((== f) . sum) $ replicateM n [0, 1]

replace :: String -> [Int] -> [Int]
replace [] [] = []
replace ('?' : r) (x : r') = x : replace r r'
replace ('.' : r) g = 0 : replace r g
replace ('#' : r) g = 1 : replace r g

ways springs groups =
  length
    . filter (check groups)
    . map (replace springs)
    $ combos unknowns missing
  where
    unknowns = length $ filter (== '?') springs
    present = length $ filter (== '#') springs
    missing = sum groups - present

part1 :: Input -> OutputA
part1 = sum . map (uncurry ways)

------------ PART B ------------
unfold :: (String, [Int]) -> (String, [Int])
unfold (s, g) = (intercalate "?" $ replicate 5 s, concat $ replicate 5 g)

-- memoized recursion stepping forward in the string
-- check if group can be satisfied and return 1 if everything does
-- and some them down the stack
ways2 s g = waysM s g
  where
    ls = length s
    lg = length g
    -- memoize an array to the function call on all possible values
    -- laziness will only evaluate it when needed, then return the result in the array
    memo =
      Array.array
        bounds
        [ ((x, y), ways' (drop x s) (drop y g))
          | x <- [0 .. ls],
            y <- [0 .. lg]
        ]
    bounds = ((0, 0), (ls, lg))

    waysM :: [Char] -> [Int] -> Int
    waysM ds dg = memo Array.! (ls - length ds, lg - length dg)

    ways' :: [Char] -> [Int] -> Int
    ways' [] [] = 1 -- all groups satisfied
    ways' [] _ = 0 -- still some groups left but no more places to fill
    ways' ('#' : _) [] = 0 -- filled but left springs damaged
    ways' ('.' : ss) gs = waysM ss gs -- already repaired
    ways' ss@('#' : _) (gg : gs) =
      if length ss >= gg -- look ahead to see if the current group can fit in the rest of the string
        && '.' `notElem` take gg ss -- make sure group only contains # or ?
        && (length ss == gg || ss !! gg /= '#') -- group must match in size and end in . or ?
        then waysM (drop (gg + 1) ss) gs -- drop the group and proceed
        else 0
    ways' ('?' : ss) gs = waysM ss gs + ways' ('#' : ss) gs

part2 :: Input -> OutputB
part2 = sum . map (uncurry ways2 . unfold)
