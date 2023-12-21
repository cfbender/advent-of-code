module Days.Day20 where

-- another one of these where
-- there is no efficient general solution
--
-- really liked part 1, really disliked part 2.
--
-- give me an Advent of Part 1s pls

import Control.Monad (foldM)
import Data.Attoparsec.Text
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe
import Program.RunDay qualified as R (Day, runDay)
import Util.Util qualified as U

runDay :: R.Day
runDay = R.runDay inputParser part1 part2

------------ PARSER ------------
inputParser :: Parser Input
inputParser = fill . M.fromList <$> (node `sepBy` endOfLine)
  where
    node = do
      name <- many1 (choice [letter, char '%', char '&'])
      string " -> "
      output <- many1 letter `sepBy` string ", "
      return (convert name output)
    convert name@('b' : rest) output = (name, Broadcaster output)
    convert name@('%' : rest) output = (rest, FlipFlop False output)
    convert name@('&' : rest) output = (rest, Conjunction M.empty output)
    fill i =
      M.mapWithKey
        ( \k v -> case v of
            Conjunction _ n ->
              let conjMap = M.map (const Low) $ M.filterWithKey (\k' v -> k `elem` neighbors v) i
               in Conjunction conjMap n
              where
                neighbors (Conjunction _ n) = n
                neighbors (FlipFlop _ n) = n
                neighbors (Broadcaster n) = n
            _ -> v
        )
        i

------------ TYPES ------------
data Pulse = Low | High deriving (Show, Eq)

data Node = Conjunction (Map String Pulse) [String] | FlipFlop Bool [String] | Broadcaster [String] deriving (Show, Eq)

------------Sent  From  Type  To
data Sent = Sent String Pulse String deriving (Show, Eq)

type Input = Map String Node

type OutputA = Int

type OutputB = String

------------ PART A ------------
pulse :: Sent -> Node -> (Node, [Sent])
-- If a flip-flop module receives a high pulse, it is ignored and nothing happens.
pulse (Sent _ High _) n@(FlipFlop _ _) = (n, [])
-- If it was off, it turns on and sends a high pulse.
pulse (Sent _ Low n) (FlipFlop False neighbors) = (FlipFlop True neighbors, map (Sent n High) neighbors)
-- If it was on, it turns off and sends a low pulse.
pulse (Sent _ Low n) (FlipFlop True neighbors) = (FlipFlop False neighbors, map (Sent n Low) neighbors)
pulse (Sent c p n') n@(Conjunction m neighbors) = (Conjunction updated neighbors, map (Sent n' pulse') neighbors)
  where
    -- the conjunction module first updates its memory for that input.
    updated = M.insert c p m
    -- if it remembers high pulses for all inputs, it sends a low pulse;
    -- otherwise, it sends a high pulse.
    pulse' = if all (== High) (M.elems updated) then Low else High
-- When a broadcaster receives a pulse, it sends the same pulse to all of its destination modules.
pulse (Sent _ p n') n@(Broadcaster neighbors) = (n, map (Sent n' p) neighbors)

perpetuate :: Input -> (Int, Int) -> [Sent] -> (Input, (Int, Int))
perpetuate i r [] = (i, r)
perpetuate i (l, h) (c@(Sent _ _ t) : rest) =
  if M.member t i
    then perpetuate updated counts (rest ++ next)
    else perpetuate i (l, h) rest
  where
    (after, next) = pulse c (i M.! t)
    -- update the node. only needed for Conjunction
    -- but easier to just do it for all
    updated = M.insert t after i
    low = length (filter (\(Sent _ p _) -> p == Low) next)
    high = length (filter (\(Sent _ p _) -> p == High) next)
    counts = (l + low, h + high)

part1 :: Input -> OutputA
part1 i =
  uncurry (*) $
    snd $
      foldl (uncurry perpetuate) initial presses
  where
    -- include 1000 pulses from button
    initial = (i, (1000, 0))
    presses = replicate 1000 [Sent "button" Low "broadcaster"]

------------ PART B ------------
allHaveCycles :: Map String (Maybe Int) -> Bool
allHaveCycles = all isJust

type Cycle = (Int, Map String (Maybe Int))

findCycle :: String -> Input -> Cycle -> [Sent] -> Either (Input, Cycle) (Input, Cycle)
findCycle s i c [] = Right (i, c)
findCycle s i (x, cycles) (c@(Sent f p t) : rest)
  | allHaveCycles cycles = Left (i, (x, cycles))
  | not $ M.member t i = findCycle s i (x, cycles) rest
  -- sent High pulse to node that feeds rx
  -- so update the sender with the cycle length
  | s == t && p == High && isNothing (cycles M.! f) = findCycle s updated (x, newCycles) (rest ++ next)
  | otherwise = findCycle s updated (x, cycles) (rest ++ next)
  where
    (after, next) = pulse c (i M.! t)
    updated = M.insert t after i
    newCycles = M.insert f (Just x) cycles

part2 :: Input -> OutputB
part2 i =
  show $
    answer $
      either id id $
        foldM
          (\(i, (x, acc)) -> findCycle feed i (x + 1, acc))
          (i, (0, feedInputs))
          (repeat [Sent "button" Low "broadcaster"])
  where
    answer = foldr (lcm . fromJust . snd) 1 . M.toList . snd . snd
    outs (Broadcaster n) = n
    outs (FlipFlop _ n) = n
    outs (Conjunction _ n) = n
    -- find the single conjunction that leads to rx
    [feed] = [name | (name, node) <- M.toList i, outs node == ["rx"]]
    -- find all the inputs to that conjunction
    feedInputs = M.fromList $ [(name, Nothing) | (name, node) <- M.toList i, outs node == [feed]]
