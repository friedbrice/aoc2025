module Day05 where

import Common
import Data.IntMap qualified as IntMap


puzzle05 :: Puzzle ([(Int, Int)], [Int])
puzzle05 = Puzzle {..}
  where
    inputPath = "resources/Day05.txt"

    sampleInput =
      "3-5\n\
      \10-14\n\
      \16-20\n\
      \12-18\n\
      \\n\
      \1\n\
      \5\n\
      \8\n\
      \11\n\
      \17\n\
      \32\n"

    sampleParsed =
      ( [(3, 5), (10, 14), (16, 20), (12, 18)]
      , [1, 5, 8, 11, 17, 32]
      )

    sampleSolutionA = 3

    sampleSolutionB = 14

    solutionA = 694

    solutionB = 352_716_206_375_547

    parseInput raw =
      let (rawRanges, rawIds) = splitInput raw
       in (fmap parseRange rawRanges, fmap (read' "puzzle05: parseInput") rawIds)

    solveA (ranges, ids) =
      fromIntegral $ length $ filter (isFresh ranges) ids

    solveB (ranges, _) =
      fromIntegral $ totalFreshIngredients $ sortedRanges ranges


splitInput :: String -> ([String], [String])
splitInput input =
  case break null (lines input) of
    (rawRanges, "" : rawIds) -> (rawRanges, rawIds)
    _ -> error $ "splitInput: invalid input" <> input


parseRange :: String -> (Int, Int)
parseRange line =
  case break (== '-') line of
    (rawStart, '-' : rawEnd)
      | Just (start, end) <- (,) <$> readMaybe rawStart <*> readMaybe rawEnd ->
          (start, end)
    _ ->
      error $ "parseRange: invalid line: " <> show line


isFresh :: [(Int, Int)] -> Int -> Bool
isFresh ranges ingredient =
  any (\(a, b) -> a <= ingredient && ingredient <= b) ranges


sortedRanges :: [(Int, Int)] -> IntMap (Max Int)
sortedRanges =
  IntMap.unionsWith (<>) . fmap (\(a, b) -> IntMap.singleton a (Max b))


totalFreshIngredients :: IntMap (Max Int) -> Int
totalFreshIngredients =
  fst . foldl' step (0, 0) . IntMap.assocs
  where
    step (count, prevEnd) (start, Max end)
      | prevEnd < start = (count + end - start + 1, end)
      | prevEnd < end = (count + end - prevEnd, end)
      | otherwise = (count, prevEnd)
