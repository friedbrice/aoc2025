module Day04 where

import Common
import Data.Set qualified as Set


puzzle04 :: Puzzle (Set (Int, Int))
puzzle04 = Puzzle {..}
  where
    inputPath = "resources/Day04.txt"

    sampleInput =
      "..@@.@@@@.\n\
      \@@@.@.@.@@\n\
      \@@@@@.@.@@\n\
      \@.@@@@..@.\n\
      \@@.@@@@.@@\n\
      \.@@@@@@@.@\n\
      \.@.@.@.@@@\n\
      \@.@@@.@@@@\n\
      \.@@@@@@@@.\n\
      \@.@.@@@.@.\n"

    sampleParsed =
      Set.fromList
        [ (0, 2)
        , (0, 3)
        , (0, 5)
        , (0, 6)
        , (0, 7)
        , (0, 8)
        , (1, 0)
        , (1, 1)
        , (1, 2)
        , (1, 4)
        , (1, 6)
        , (1, 8)
        , (1, 9)
        , (2, 0)
        , (2, 1)
        , (2, 2)
        , (2, 3)
        , (2, 4)
        , (2, 6)
        , (2, 8)
        , (2, 9)
        , (3, 0)
        , (3, 2)
        , (3, 3)
        , (3, 4)
        , (3, 5)
        , (3, 8)
        , (4, 0)
        , (4, 1)
        , (4, 3)
        , (4, 4)
        , (4, 5)
        , (4, 6)
        , (4, 8)
        , (4, 9)
        , (5, 1)
        , (5, 2)
        , (5, 3)
        , (5, 4)
        , (5, 5)
        , (5, 6)
        , (5, 7)
        , (5, 9)
        , (6, 1)
        , (6, 3)
        , (6, 5)
        , (6, 7)
        , (6, 8)
        , (6, 9)
        , (7, 0)
        , (7, 2)
        , (7, 3)
        , (7, 4)
        , (7, 6)
        , (7, 7)
        , (7, 8)
        , (7, 9)
        , (8, 1)
        , (8, 2)
        , (8, 3)
        , (8, 4)
        , (8, 5)
        , (8, 6)
        , (8, 7)
        , (8, 8)
        , (9, 0)
        , (9, 2)
        , (9, 4)
        , (9, 5)
        , (9, 6)
        , (9, 8)
        ]

    sampleSolutionA = 13

    sampleSolutionB = 43

    solutionA = 1349

    solutionB = 8277

    parseInput cs =
      foldMap asSet tuples
      where
        tuples =
          cs
            & lines
            & zip [0 ..]
            & (fmap . fmap) (zip [0 ..])
            & fmap sequence
            & join

        asSet (row, (col, '@')) = Set.singleton (row, col)
        asSet _ = mempty

    solveA = countRemovableRolls

    solveB = countAllRemovableRolls


countAdjacentRolls :: Set (Int, Int) -> (Int, Int) -> Int
countAdjacentRolls rolls pos =
  length $ filter (`Set.member` rolls) $ adjacentPositions pos


adjacentPositions :: (Int, Int) -> [(Int, Int)]
adjacentPositions (row, col) =
  [ (row - 1, col - 1)
  , (row - 1, col)
  , (row - 1, col + 1)
  , (row, col - 1)
  , (row, col + 1)
  , (row + 1, col - 1)
  , (row + 1, col)
  , (row + 1, col + 1)
  ]


countRemovableRolls :: Set (Int, Int) -> Integer
countRemovableRolls rolls =
  getSum $ foldMap (countRoll rolls) rolls
  where
    countRoll rs pos = if countAdjacentRolls rs pos < 4 then 1 else 0


countAllRemovableRolls :: Set (Int, Int) -> Integer
countAllRemovableRolls rolls =
  fromIntegral $ Set.size rolls - Set.size leftovers
  where
    leftovers = removeRolls rolls

    removeRolls rs =
      let rs' = Set.filter (\pos -> countAdjacentRolls rs pos >= 4) rs
       in if rs' == rs
            then rs
            else removeRolls rs'
