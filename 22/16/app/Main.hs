{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE Strict #-}

import           Control.Applicative
import           Control.Composition
import           Control.Monad
import qualified Control.Monad.State.Strict    as ST
import           Data.Either
import           Data.Either.Combinators        ( fromRight' )
import           Data.List
import           Data.List.Index
import           Data.List.Tools
import qualified Data.Map.Strict               as M
import qualified Data.Set                      as S
import           Data.Tuple.Extra
import qualified Data.Vector.Unboxed           as V
import           Data.Void
import           Debug.Trace
import           Safe
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer    as L

type Parser a = Parsec Void String a

traceI :: Show a => a -> a
traceI a = trace (show a) a

main :: IO ()
main = do
  file <- readFile "input"
  print @Int $ calculate file

ez = False

calculate :: String -> Int
calculate fl = if ez
  then fst $ ST.evalState (travel 30 S.empty 0 0) M.empty
  else
    liftA2 ($) (\a b -> foldl' b 0 a) fld
    $ traceI$fmap (swap . snd)
    $ filter ((== 0) . fst3 . fst)
    $ M.toList
    $ ST.execState (travel 26 S.empty 0 0) M.empty
 where
  fld :: [(S.Set Int, Int)] -> Int -> (S.Set Int, Int) -> Int
  fld list mx val = foldl'
    (\mxx k -> if (S.intersection (fst val) (fst k) == S.empty)
      then max (snd val + snd k) mxx
      else mxx
    )
    mx
    list
  parsed  = fromRight' $ parse systemP "" fl
  all     = fmap fst parsed
  edgeMap = M.fromList
    $ fmap ((second (second $ fmap nameToInt)) . (first nameToInt)) parsed
  nameToInt a = elemIndexJust a all
  travel :: Int -> S.Set Int -> Int -> Int -> ST.State Cache (Int, S.Set Int)
  travel 0 vis score place = do
    ST.modify (M.insertWith max (0, vis, -1) (score, vis))
    pure (0, S.empty)
  travel (-1) vis score place = do
    ST.modify (M.insertWith max (0, vis, -1) (score, vis))
    pure (0, S.empty)
  travel time vis score place = do
            fnd <- found
            case fnd of 
     (Just ls)-> do
        traverse (finalInsert (vis,score)) ls
      _ -> do
        (mxx, vis3) <- mx
        ST.modify (M.insertWith max (time, vis, place) (mxx, vis3))
        pure (mxx, vis3)
   where
    found :: ST.State Cache (Maybe (Int, S.Set Int))
    found = do
      mp <- ST.get
      pure $ mp M.!? (time, vis, place)

    mx :: ST.State Cache (Int, S.Set Int)
    mx = do
      mp      <- ST.get
      opValve <-
        (if not (S.member place vis) && rate > 0
          then traverse
            ( fmap ((second (S.insert place)) . (first (+ valveOp)))
            . travel (time - 2) (S.insert place vis) (score + valveOp)
            )
            edges
          else pure []
        )
      noValve <- traverse (travel (time - 1) vis score) edges
      pure $ head $ reverse$sortOn fst (noValve ++ opValve)
    valveOp       = (time - 1) * rate
    (rate, edges) = edgeMap M.! place 
    finalInsert (score1,vis1) (score2,vis2)=do
                    ST.modify (M.insertWith max (0, S.union vis1 vis2, -1) (score1 +score2, S.union vis1 vis2))
type Cache = M.Map (Int, S.Set Int, Int) [(Int, S.Set Int)]

systemP = caveP `sepBy` string ""

caveP = do
  string "Valve "
  name <- nameP
  string " has flow rate="
  flow <- L.decimal
  try (string "; tunnels lead to valves ") <|> string "; tunnel leads to valve "
  others <- nameP `sepBy` string ", "
  newline
  pure (name, (flow, others))

nameP :: Parser String
nameP = do
  n1 <- L.charLiteral
  n2 <- L.charLiteral
  return [n1, n2]
