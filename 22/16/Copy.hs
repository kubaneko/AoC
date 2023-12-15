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
import qualified          Control.Monad.State.Strict as ST
import           Data.Either
import           Data.Either.Combinators        ( fromRight' )
import           Data.List
import           Data.List.Index
import           Data.List.Tools
import qualified          Data.Map.Strict as M
import qualified Data.Set                      as S
import           Data.Tuple.Extra
import           Data.Void
import        qualified   Data.Vector.Unboxed as V
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

calculate fl = ST.evalState  (travel True 26 S.empty 0) M.empty
 where
  parsed    = fromRight' $ parse systemP "" fl
  all = fmap fst parsed 
  edgeMap = M.fromList$fmap ((second (second$fmap nameToInt)).(first nameToInt)) parsed
  nameToInt a = elemIndexJust a all
  pathLimit = 30
  travel :: Bool->Int -> S.Set Int-> Int -> ST.State Cache Int
  travel False 0  _ _ = pure 0
  travel False (-1) _ _= pure 0
  travel True (-1) vis _= do
                    st<-ST.get
                    travel False 26 vis 0
  travel True 0  vis _= do
                    st<-ST.get
                    travel False 26 vis 0
  travel again time vis place = do
        fnd <- found
        case fnd of
            (Just a) -> pure a
            _ -> do
                mxx<-mx
                ST.modify (if S.size vis<15 then (M.insert (again,time,vis,place) mxx) else id)
                return mxx
   where
    found = do
            mp<-ST.get
            pure$mp M.!? (again,time, vis, place)

    mx = do
        mp<-ST.get
        opValve<- (if not (S.member place vis) && rate > 0
          then 
            traverse (fmap (+ valveOp) . travel again (time - 2) (S.insert place vis)) edges
          else pure [])
        noValve<-traverse (travel again (time - 1) vis ) edges
        pure$maximum (noValve ++ opValve)
    valveOp       = (time - 1) * rate
    (rate, edges) = edgeMap M.! place

type Cache = M.Map (Bool,Int, S.Set Int, Int) Int

systemP = caveP `sepBy` string ""

caveP = do
  string "Valve "
  name <- nameP
  string " has flow rate="
  flow <- L.decimal
  try ( string "; tunnels lead to valves ")
    <|> string "; tunnel leads to valve "
  others <- nameP `sepBy` string ", "
  newline
  pure (name, (flow, others))

nameP :: Parser String
nameP = do
  n1 <- L.charLiteral
  n2 <- L.charLiteral
  return [n1, n2]
