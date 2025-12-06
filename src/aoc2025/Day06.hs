module Day06 where

import Common
import Data.List qualified as List
import Data.List.Split


puzzle06 :: Puzzle String
puzzle06 = Puzzle {..}
  where
    inputPath = "resources/Day06.txt"

    sampleInput =
      "123 328  51 64 \n\
      \ 45 64  387 23 \n\
      \  6 98  215 314\n\
      \*   +   *   +  \n"

    sampleParsed = sampleInput

    sampleSolutionA = 4_277_556

    sampleSolutionB = 3_263_827

    solutionA = 6_378_679_666_679

    solutionB = 11_494_432_585_168

    parseInput = id

    solveA = sum . fmap computeRows . parseRows

    solveB = sum . fmap computeCols . parseCols


parseRows :: String -> [[String]]
parseRows = List.transpose . fmap words . lines


computeRows :: [String] -> Integer
computeRows row = case (init row, last row) of
  (nums, "*") | Just nums' <- traverse readMaybe nums -> product nums'
  (nums, "+") | Just nums' <- traverse readMaybe nums -> sum nums'
  _ -> error $ "computeRows: invalid row " <> show row


parseCols :: String -> [(String, [Integer])]
parseCols raw =
  zip ops nums
  where
    foo = raw & lines
    nums = init foo & List.transpose & fmap trim & splitOn [""] & fmap (fmap read)
    ops = last foo & words


computeCols :: (String, [Integer]) -> Integer
computeCols ("+", cols) = sum cols
computeCols ("*", cols) = product cols
computeCols x = error $ "computeCols: invalid input " <> show x
