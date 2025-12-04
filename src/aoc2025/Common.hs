module Common (
  module Common,
  module Control.Applicative,
  module Control.Monad,
  module Data.Function,
  module Data.Functor.Identity,
  module Data.Maybe,
  module Data.Monoid,
  module Data.Void,
  module Debug.Trace,
  HasCallStack,
  M.Map,
  S.Set,
  T.Text,
  readMaybe,
) where

import Control.Applicative
import Control.Monad
import Data.Function
import Data.Functor.Identity
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Monoid
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Void
import Debug.Trace
import GHC.Stack (HasCallStack)
import Text.Read


powerList :: [a] -> [[a]]
powerList [] = [[]]
powerList (x : xs) = let ps = powerList xs in [x : p | p <- ps] <> ps


data Puzzle a = Puzzle
  { inputPath :: FilePath
  , sampleInput :: String
  , sampleParsed :: a
  , sampleSolutionA :: Integer
  , sampleSolutionB :: Integer
  , solutionA :: Integer
  , solutionB :: Integer
  , parseInput :: String -> a
  , solveA :: a -> Integer
  , solveB :: a -> Integer
  }


testParseInput :: (Eq a) => Puzzle a -> Bool
testParseInput p = p.parseInput p.sampleInput == p.sampleParsed


testSolveA :: Puzzle a -> Bool
testSolveA p = p.solveA p.sampleParsed == p.sampleSolutionA


testSolveB :: Puzzle a -> Bool
testSolveB p = p.solveB p.sampleParsed == p.sampleSolutionB


solvePartA :: Puzzle a -> IO Integer
solvePartA p = (p.solveA . p.parseInput) <$> readFile p.inputPath


solvePartB :: Puzzle a -> IO Integer
solvePartB p = (p.solveB . p.parseInput) <$> readFile p.inputPath


runPuzzle :: Puzzle a -> IO ()
runPuzzle p = do
  resultA <- solvePartA p
  resultB <- solvePartB p
  putStr $ p.inputPath <> ": "
  print (resultA, resultA == p.solutionA, resultB, resultB == p.solutionB)
