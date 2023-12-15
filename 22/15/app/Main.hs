{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

import           Control.Applicative
import           Control.Composition
import           Control.Monad
import           Data.Either
import           Data.Either.Combinators        ( fromRight' )
import           Data.List
import           Data.List.Index
import           Data.List.Tools
import           Data.Maybe

-- import           Data.List.Split
import           Data.Tuple.Extra
import           Data.Void
import           Debug.Trace
import           Safe
import           Text.Megaparsec
import qualified Text.Megaparsec               as M
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer    as L

type Parser a = Parsec Void String a

traceI :: Show a => a -> a
traceI a = trace (show a) a

main :: IO ()
main = do
  file <- readFile "input"
  print $ calculate file

ez = False

calculate fl = if ez
  then
    sum
    $   liftA2 (-) (fst . snd) (fst . fst)
    <$> (merge $ sortOn (fst . fst) $ bounds intrY)
  else
    (\case
      (Just x, y) -> 4000000 * x + y
    )
    $   findJust (isJust . fst)
    $   liftA2
          (,)
          ( maybeSpace
          . catMaybes
          . fmap (checkBounds)
          . merge
          . sortOn (fst . fst)
          . bounds
          )
          (id)
    <$> [0 .. boundY]

 where
  merge (((a, b), (c, d)) : ((e, f), (g, h)) : xs)
    | e <= c && g > c = merge (((a, b), (g, h)) : xs)
    | e <= c          = merge (((a, b), (c, d)) : xs)
    | otherwise       = ((a, b), (c, d)) : merge xs
  merge a = a
  parsed = fromRight' $ parse allP "" fl
  intrY  = 2000000
  boundY = 4000000
  bounds line =
    filter (liftA2 (<=) (fst . fst) (fst . snd))
      $   (\((x, y), b) ->
            ((x - b + abs (line - y), line), (x + b - abs (line - y), line))
          )
      <$> parsed
  checkBounds ((a, b), (c, d)) | a < 0 && c < 0 = Nothing
                               | a > boundY && c > boundY = Nothing
                               | a < 0 = Just ((0, b), (c, d))
                               | c > boundY = Just ((a, b), (boundY, d))
                               | otherwise = Just ((a, b), (c, d))
  maybeSpace a = maybeSpaceInd
    ( ((-1, snd $ snd $ head a), (-1, snd $ snd $ head a))
    : (  a
      ++ [ ( (boundY + 1, snd $ snd $ head a)
           , (boundY + 1, snd $ snd $ head a)
           )
         ]
      )
    )
  maybeSpaceInd :: [((Int, Int), (Int, Int))] -> Maybe Int
  maybeSpaceInd (a : b : xs) = if ((fst $ snd a) + 1 < (fst $ fst b))
    then Just ((fst $ snd a) + 1)
    else maybeSpaceInd (b : xs)
  maybeSpaceInd _ = Nothing



allP :: Parser [((Int, Int), Int)]
allP = senbeaP `sepBy` string ""

int = L.signed space L.decimal

senbeaP = do
  xs <- string "Sensor at x=" *> int
  ys <- string ", y=" *> int
  xb <- string ": closest beacon is at x=" *> int
  yb <- string ", y=" *> int <* newline
  return ((xs, ys), abs (xs - xb) + abs (ys - yb))
