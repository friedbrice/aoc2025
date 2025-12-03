module Day01 where

import Common


puzzle01 :: Puzzle [Turn]
puzzle01 = Puzzle {..}
  where
    inputPath = "resources/Day01.txt"

    sampleInput =
      "L68\n\
      \L30\n\
      \R48\n\
      \L5\n\
      \R60\n\
      \L55\n\
      \L1\n\
      \L99\n\
      \R14\n\
      \L82\n"

    sampleParsed =
      [ L 68
      , L 30
      , R 48
      , L 5
      , R 60
      , L 55
      , L 1
      , L 99
      , R 14
      , L 82
      ]

    sampleSolutionA = 3

    sampleSolutionB = 6

    solutionA = 1195

    solutionB = 6770

    parseInput = fmap parseLine . lines
      where
        parseLine ('R' : digs) | Just n <- readMaybe digs = R n
        parseLine ('L' : digs) | Just n <- readMaybe digs = L n
        parseLine l = error $ "parseInput01: Invalid line: " <> l

    solveA = fst . countEndingZeros 50

    solveB = fst . countRollingZeros 50


data Turn = L Integer | R Integer
  deriving (Show, Eq)


countEndingZeros :: Integer -> [Turn] -> (Integer, Integer)
countEndingZeros start deltas = foldl' step (0, start) deltas
  where
    step (cnt, acc) delta =
      let acc' = (acc + case delta of R n -> n; L n -> negate n) `mod` 100
       in if acc' == 0
            then (cnt + 1, acc')
            else (cnt, acc')


turn :: Turn -> (Integer, Integer) -> (Integer, Integer)
turn (L 0) (cnt, pos) = (cnt, pos)
turn (R 0) (cnt, pos) = (cnt, pos)
turn (L n) (cnt, pos) = turn (L (n - 1)) (cnt', pos')
  where
    pos' = (pos - 1) `mod` 100
    cnt' = if pos' == 0 then cnt + 1 else cnt
turn (R n) (cnt, pos) = turn (R (n - 1)) (cnt', pos')
  where
    pos' = (pos + 1) `mod` 100
    cnt' = if pos' == 0 then cnt + 1 else cnt


countRollingZeros :: Integer -> [Turn] -> (Integer, Integer)
countRollingZeros start deltas = foldl' (flip turn) (0, start) deltas
