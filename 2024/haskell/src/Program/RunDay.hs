module Program.RunDay (runDay, Day, Verbosity (None, Quiet, Timings, Verbose), RunType (Part1, Part2, Both)) where

import Control.Exception (SomeException, catch, evaluate)
import Control.Monad (when)
import Control.Monad.Except
import Control.Monad.IO.Class
import Data.Attoparsec.Text
import Data.Maybe
import Data.Text (pack)
import Data.Time (diffUTCTime, getCurrentTime)
import Program.Color
import System.Console.ANSI
import System.Directory (doesFileExist)
import Text.Printf

data Verbosity = None | Quiet | Timings | Verbose deriving (Eq, Show, Ord)

data RunType = Part1 | Part2 | Both deriving (Eq, Show)

type Day = Verbosity -> String -> RunType -> IO (Maybe Double, Maybe Double)

runDay :: (Show a, Show b, Show i) => Parser i -> (i -> a) -> (i -> b) -> Program.RunDay.Day
runDay inputParser part1 part2 verbosity inputFile runType = do
  input <- runExceptT $ do
    inputFileExists <- liftIO $ doesFileExist inputFile
    fileContents <-
      if inputFileExists
        then liftIO $ readFile inputFile
        else
          throwError $
            unwords
              [ "I couldn't read the input!"
              , "I was expecting it to be at"
              , inputFile
              ]
    case parseOnly inputParser . pack $ fileContents of
      Left e -> throwError $ "Parser failed to read input. Error:\n" ++ e
      Right i -> do
        when (verbosity == Verbose) $ do
          liftIO $ putStrLn "Parser output:"
          liftIO $ print i
        return i

  case input of
    Left x -> withColor Red (putStrLn x) >> return (Nothing, Nothing)
    Right i -> do
      (successA, time1) <-
        if runType `elem` [Part1, Both]
          then do
            when (verbosity >= Quiet) $ withColor Blue $ putStrLn "Part 1:"
            time1 <- getCurrentTime
            (successA, resultA) <- catch (return (True, Just (part1 i))) $
              \(m :: SomeException) -> withColor Red $ do
                putStrLn "Couldn't run Part 1!"
                when (verbosity == Verbose) $ print m
                return (False, Nothing)
            time2 <- getCurrentTime
            when (successA && verbosity > None) $ print (fromJust resultA)

            let time1' = realToFrac $ diffUTCTime time2 time1
            when (verbosity >= Timings && successA) $ putStrLn $ printf "(%.2fs)" time1'
            return (successA, time1')
          else do
            when (verbosity >= Quiet) $ print "Skipping part 1."
            return (False, 0)

      (successB, time2) <-
        if runType `elem` [Part2, Both]
          then do
            when (verbosity >= Quiet) $ withColor Blue $ putStrLn "Part 2:"
            time2 <- getCurrentTime
            (successB, resultB) <- catch (return (True, Just (part2 i))) $
              \(m :: SomeException) -> withColor Red $ do
                putStrLn "Couldn't run Part 2!"
                when (verbosity == Verbose) $ print m
                return (False, Nothing)
            time3 <- getCurrentTime
            when (successB && verbosity > None) $ print (fromJust resultB)

            let time2' = realToFrac $ diffUTCTime time3 time2
            when (verbosity >= Timings && successB) $ putStrLn $ printf "(%.2fs)" time2'
            return (successB, time2')
          else do
            when (verbosity >= Quiet) $ print "Skipping part 2."
            return (False, 0)

      return $
        (,)
          (if successA then Just time1 else Nothing)
          (if successB then Just time2 else Nothing)
