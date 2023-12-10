module Program.RunDay (runDay, Day, Verbosity (None, Quiet, Timings, Verbose)) where

import Control.Exception (SomeException, catch, evaluate)
import Control.Monad.Except
import Data.Attoparsec.Text
import Data.Text (pack)
import Data.Time (diffUTCTime, getCurrentTime)
import Program.Color
import System.Console.ANSI
import System.Directory (doesFileExist)
import Text.Printf

data Verbosity = None | Quiet | Timings | Verbose deriving (Eq, Show, Ord)

type Day = Verbosity -> String -> IO (Maybe Double, Maybe Double)

runDay :: (Show a, Show b, Show i) => Parser i -> (i -> a) -> (i -> b) -> Program.RunDay.Day
runDay inputParser partA partB verbosity inputFile = do
  input <- runExceptT $ do
    inputFileExists <- liftIO $ doesFileExist inputFile
    fileContents <-
      if inputFileExists
        then liftIO $ readFile inputFile
        else
          throwError $
            unwords
              [ "I couldn't read the input!",
                "I was expecting it to be at",
                inputFile
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
      when (verbosity >= Quiet) $ withColor Blue $ putStrLn "Part A:"
      time1 <- getCurrentTime
      (successA, resultA) <- catch (return (True, Just (partA i))) $
        \(m :: SomeException) -> withColor Red $ do
          putStrLn "Couldn't run Part A!"
          when (verbosity == Verbose) $ print m
          return (False, Nothing)
      time2 <- getCurrentTime
      when (successA && verbosity > None) $ print resultA

      let timeA = realToFrac $ diffUTCTime time2 time1
      when (verbosity >= Timings && successA) $ putStrLn $ printf "(%.2fs)" timeA

      when (verbosity >= Quiet) $ withColor Blue $ putStrLn "Part B:"
      (successB, resultB) <- catch (return (True, Just (partB i))) $
        \(m :: SomeException) -> withColor Red $ do
          putStrLn "Couldn't run Part B!"
          when (verbosity == Verbose) $ print m
          return (False, Nothing)
      time3 <- getCurrentTime
      when (successB && verbosity > None) $ print resultB

      let timeB = realToFrac $ diffUTCTime time3 time2
      when (verbosity >= Timings && successB) $ putStrLn $ printf "(%.2fs)" timeB

      return $
        (,)
          (if successA then Just timeA else Nothing)
          (if successB then Just timeB else Nothing)
