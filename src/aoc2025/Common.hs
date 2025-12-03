module Common (
  module Common,
  module Control.Applicative,
  module Control.Monad,
  module Data.Function,
  module Data.Functor.Identity,
  module Data.Maybe,
  module Data.Void,
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
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Void
import GHC.Stack (HasCallStack)
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P
import Text.Read


pipe :: (a -> b -> c) -> (a' -> a) -> (b' -> b) -> (c -> c') -> a' -> b' -> c'
pipe op f g h x y = h (op (f x) (g y))


removeAt :: Int -> [a] -> [a]
removeAt i xs = case Prelude.splitAt i xs of
  (pfx, _ : sfx) -> pfx <> sfx
  (_, []) -> error $ "Remove: Out of bounds: " <> Prelude.show i


newtype Parser a = Parser {unParser :: P.ParsecT Void T.Text Identity a}
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , Alternative
    , MonadPlus
    , MonadFail
    , P.MonadParsec Void T.Text
    )


runParser :: Parser a -> FilePath -> T.Text -> Either (P.ParseErrorBundle T.Text Void) a
runParser (Parser p) fp i = P.runParser p fp i


int :: Parser Int
int =
  some P.digitChar >>= \ds -> case readMaybe ds of
    Nothing -> fail $ "int: cannot read digits: " <> show ds
    Just x -> pure x


anyChar :: Parser Char
anyChar = P.token Just mempty


parseTest :: (Show a) => Parser a -> T.Text -> IO ()
parseTest (Parser p) = P.parseTest p


partition :: (a -> Either b c) -> [a] -> ([b], [c])
partition _ [] = ([], [])
partition f (x : xs) =
  case f x of
    Left b -> let (bs, cs) = partition f xs in (b : bs, cs)
    Right c -> let (bs, cs) = partition f xs in (bs, c : cs)


data Puzzle a = Puzzle
  { inputPath :: FilePath
  , sampleInput :: String
  , sampleParsed :: a
  , sampleSolutionA :: Int
  , sampleSolutionB :: Int
  , solutionA :: Int
  , solutionB :: Int
  , parseInput :: String -> a
  , solveA :: a -> Int
  , solveB :: a -> Int
  }


testParseInput :: Eq a => Puzzle a -> Bool
testParseInput p = p.parseInput p.sampleInput == p.sampleParsed


testSolveA :: Puzzle a -> Bool
testSolveA p = p.solveA p.sampleParsed == p.sampleSolutionA


testSolveB :: Puzzle a -> Bool
testSolveB p = p.solveB p.sampleParsed == p.sampleSolutionB


solvePartA :: Puzzle a -> IO Int
solvePartA p = (p.solveA . p.parseInput) <$> readFile p.inputPath


solvePartB :: Puzzle a -> IO Int
solvePartB p = (p.solveB . p.parseInput) <$> readFile p.inputPath


runPuzzle :: Puzzle a -> IO ()
runPuzzle p = do
  resultA <- solvePartA p
  resultB <- solvePartB p
  putStr $ p.inputPath <> ": "
  print (resultA, resultA == p.solutionA, resultB, resultB == p.solutionB)
