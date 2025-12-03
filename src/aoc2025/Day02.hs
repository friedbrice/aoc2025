module Day02 where

import Common
import Data.List.Split


puzzle02 :: Puzzle [(Int, Int)]
puzzle02 = Puzzle {..}
  where
    inputPath = "resources/Day02.txt"

    sampleInput =
      "11-22,\
      \95-115,\
      \998-1012,\
      \1188511880-1188511890,\
      \222220-222224,\
      \1698522-1698528,\
      \446443-446449,\
      \38593856-38593862,\
      \565653-565659,\
      \824824821-824824827,\
      \2121212118-2121212124"

    sampleParsed =
      [ (11, 22)
      , (95, 115)
      , (998, 1012)
      , (1188511880, 1188511890)
      , (222220, 222224)
      , (1698522, 1698528)
      , (446443, 446449)
      , (38593856, 38593862)
      , (565653, 565659)
      , (824824821, 824824827)
      , (2121212118, 2121212124)
      ]

    sampleSolutionA = 1_227_775_554

    sampleSolutionB = 4_174_379_265

    solutionA = 40_398_804_950

    solutionB = 65_794_984_339

    parseInput = fmap (parseTuple . splitOn "-") . splitOn ","
      where
        parseTuple [a, b]
          | Just (a', b') <- (,) <$> readMaybe a <*> readMaybe b = (a', b')
        parseTuple x =
          error $ "parseInput02: Invalid tuple: " <> show x

    solveA = sum . fmap (sumInvalids simpleInvalid)

    solveB = sum . fmap (sumInvalids complexInvalid)


simpleInvalid :: Int -> Bool
simpleInvalid n = front == back
  where
    digs = show n
    len = length digs
    (front, back) = splitAt (len `div` 2) digs


complexInvalid :: Int -> Bool
complexInvalid = any allEqual . allChunks . show
  where
    allEqual [] = True
    allEqual (x : xs) = all (== x) xs

    allChunks xs = [chunksOf d xs | d <- allDivisors (length xs)]

    allDivisors k = filter (\d -> k `mod` d == 0) [1 .. k `div` 2]


sumInvalids :: (Int -> Bool) -> (Int, Int) -> Int
sumInvalids isInvalid (a, b) = sum $ filter isInvalid [a .. b]
