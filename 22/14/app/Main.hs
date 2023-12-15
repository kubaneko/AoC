{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

import           Control.Applicative
import           Control.Composition
import           Control.Monad
import           Control.Monad.Loops
import           Control.Monad.State.Lazy
import           Data.Either
import           Data.Either.Combinators        ( fromRight' )
import           Data.List
import           Data.List.Index
import           Data.List.Tools
import           Data.Maybe
import qualified Data.Set                      as S

import           Data.List.Split
import           Data.Ord.HT                    ( inRange )
import           Data.Tuple.Extra
import           Data.Tuple.HT                  ( sortPair )
import           Data.Void
import           Debug.Trace
import           Safe
import           Text.Megaparsec

type Parser a = Parsec Void String a

traceI :: Show a => a -> a
traceI a = trace (show a) a

main :: IO ()
main = do
  file <- readFile "input"
  print $ calculate (lines file)

ez = False

adjToList [x         ] = []
adjToList (x : y : xs) = [x, y] : adjToList (y : xs)

firstPair (x : y : xs) = (x, y)

calculate lns =
  length $ fst $ findJust (uncurry (==)) $ liftA2 zip (drop 1) id $ iterate
    (newSand bottom sandStart lines)
    S.empty
 where
  lines = firstPair <$> concat
    (   fmap (fmap (firstPair . sort) . transpose)
    .   adjToList
    .   fmap (fmap (read @Int) . splitOn ",")
    .   splitOn " -> "
    <$> lns
    )
  sandStart = (500, 0)
  bottom :: Int
  bottom = foldl' (\a b -> max a $ snd $ snd b) 0 lines

-- second is the line
collidesLine (x, y) (a, b) = inRange a x && inRange b y
collidesGen lines sand x =
  (x `S.member` sand) || or (fmap (collidesLine x) lines)

newSand
  :: Int
  -> (Int, Int)
  -> [((Int, Int), (Int, Int))]
  -> S.Set (Int, Int)
  -> S.Set (Int, Int)
newSand bot gen lines sand = if (gen `S.member` sand) || isNothing new
  then sand
  else S.insert (fromJust new) sand
 where
  new   = dropSand gen
  diffs = [(0, 1), (-1, 1), (1, 1)]
  dropSand :: (Int, Int) -> Maybe (Int, Int)
  dropSand x | snd x > bot   = if ez then Nothing else Just x
             | isNothing ind = Just x
             | otherwise     = dropSand (add x (diffs !! fromJust ind))
    where ind = elemIndex False $ fmap (collidesGen lines sand . add x) diffs

add (a, b) (c, d) = (a + c, b + d)
