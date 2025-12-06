module Day03 where

import Common


puzzle03 :: Puzzle [[Integer]]
puzzle03 = Puzzle {..}
  where
    inputPath = "resources/Day03.txt"

    sampleInput =
      "987654321111111\n\
      \811111111111119\n\
      \234234234234278\n\
      \818181911112111\n"

    sampleParsed =
      [ [9, 8, 7, 6, 5, 4, 3, 2, 1, 1, 1, 1, 1, 1, 1]
      , [8, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 9]
      , [2, 3, 4, 2, 3, 4, 2, 3, 4, 2, 3, 4, 2, 7, 8]
      , [8, 1, 8, 1, 8, 1, 9, 1, 1, 1, 1, 2, 1, 1, 1]
      ]

    sampleSolutionA = 357

    sampleSolutionB = 3_121_910_778_619

    solutionA = 16993

    solutionB = 168_617_068_915_447

    parseInput = (fmap . fmap) (read' "puzzle03: parseInput" . pure) . lines

    solveA = sum . fmap maxJoltage2

    solveB = sum . fmap maxJoltage12'


maxJoltage2 :: [Integer] -> Integer
maxJoltage2 [x, y] = 10 * x + y
maxJoltage2 (x : ys) = maximum [10 * x + y | y <- ys] `max` maxJoltage2 ys
maxJoltage2 rest = error $ "maxJoltage: Invalid input: " <> show rest


maxJoltage12 :: [Integer] -> Integer
maxJoltage12 xs =
  maximum [read' "maxJoltage12" (join $ fmap show cs) | cs <- candidates]
  where
    candidates = filter ((12 ==) . length) (powerList xs)


-- https://www.reddit.com/r/haskell/comments/1pcvbid/comment/ns3b82b/
maxJoltage12' :: [Integer] -> Integer
maxJoltage12' =
  read' "maxJoltage12'" . selectMax 12 . withIndex . reverse . asChars
  where
    asChars = join . fmap show

    withIndex = flip zip [0 ..]

    selectMax :: Int -> [(Char, Int)] -> [Char]
    selectMax 0 _ = []
    selectMax n xs = d : selectMax (n - 1) (take i xs)
      where
        (d, i) = maximum (drop (n - 1) xs)
