module Days.Day11 where

{- ORMOLU_DISABLE -}
import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text (Parser, sepBy, endOfLine)
import Data.Void
import Data.List
import Util.Parsers (coordinateParser)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Vector (Vector)
import qualified Data.Vector as Vector
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser =
  Vector.fromList
    . map (Vector.fromList . map snd)
    . transpose
    . groupBy (\((xA, _), _) ((xB, _), _) -> xA == xB)
    . Map.toList
    <$> coordinateParser space 0
  where
    space '#' = Just Galaxy
    space '.' = Just Empty

------------ TYPES ------------
data Space = Empty | Galaxy deriving (Show, Eq)

type Coordinate = (Int, Int)

type Input = Vector (Vector Space)

type OutputA = String

type OutputB = Void

------------ PART A ------------
-- TODO: splitAt each empty, insert new empty row/col, then calculate manhattan distance on new grid
partA :: Input -> OutputA
partA i = show emptyCols
  where
    transposed = Vector.fromList . map Vector.fromList . transpose . map Vector.toList $ Vector.toList i
    emptyRows = Vector.filter (all (== Empty) . snd) $ Vector.indexed i
    emptyCols = Vector.filter (all (== Empty) . snd) $ Vector.indexed transposed

------------ PART B ------------
partB :: Input -> OutputB
partB = error "Not implemented yet!"
