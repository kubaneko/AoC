{-# LANGUAGE Strict #-}
module Main where

import Data.Char
import Data.Maybe
import Control.Monad
import Data.Tuple.Extra (fst3, snd3)
import Data.List.Extra (groupOn)
import Data.List
import Debug.Trace
import qualified Data.Vector as V
import Data.Vector ((!), (!?))
import qualified Data.Map.Strict as M
import Data.List.Split (splitOn)
import Control.Monad.Trans.State.Strict
import Data.List.Utils (dropWhileList)
import Algorithm.Search

main :: IO ()
main = do
  i <- readFile "input"
  print$ solve i

solve :: String -> Integer
solve i = (\case (a,b) -> a*b)$(\x -> evalState x parsed)$do
  xs <- traverse (\a -> sendSignal$traceShowId a) (replicate 100000000000 [Signal "" "broadcaster" False])
  return$foldl' (pairPl) (0,0) xs
  where
    parsed = M.fromList$fmap fFun$lines i
    outInp :: [(String,[String])]
    outInp = fmap ((\case (nm: _ : outp) -> (dropWhile (not.isAlpha) nm,fmap (filter (/=','))$outp)).words)$lines i
    fFun :: String -> (String,Oper)
    fFun ('&':rst) = (\case (nm: _ : outp) -> (nm,)$Oper nm ((fmap (filter (/=',')))$outp) (Conj (M.fromList$fmap (,False)$fmap fst$filter (elem nm.snd) outInp)))$words rst
    fFun ('%':rst) = (\case (nm: _ : outp) -> (nm,)$Oper nm ((fmap (filter (/=',')))$outp) (Flip False))$words rst
    fFun xs = (\case (nm: _ : outp) -> (nm,)$Oper nm (fmap (filter (/=','))$outp) Simple)$words xs

data Type = Conj (M.Map String Bool) | Simple | Flip Bool

data Oper = Oper{ name :: String, outputs :: [String], gType :: Type }

data Signal = Signal String String Bool
  deriving (Show)

sendSignal [] = return (0,0)
sendSignal xs = do
        mop <- gets (M.!? nm)
        case mop of
          Nothing -> sendSignal (init xs) >>= (\x->return$pairPl x (if high then (1,0) else traceShow (nm,high) (0,1)))
          Just op -> do
                (sgs, op') <- return$ receiveSignal sig op
                modify (M.adjust (const op') nm)
                p <- sendSignal ((reverse sgs) ++ init xs)
                return$pairPl (if high then (1,0) else (0,1)) p
        where
          nm = (\case Signal _ nm _ -> nm) sig
          high = (\case Signal _ _ high -> high) sig
          sig = last xs


receiveSignal (Signal _ _ high) op@(Oper nm outs Simple) = (fmap (\x -> Signal nm x high) outs, op)
receiveSignal (Signal _ _ high) op@(Oper nm outs (Flip on))
  | high = ([], op)
  | otherwise = (fmap (\x -> Signal nm x (not on)) outs, (Oper nm outs (Flip (not on))))
receiveSignal (Signal from _ high) op@(Oper nm outs (Conj mp)) = (fmap (\x -> Signal nm x signal) outs, op{gType = Conj mp'})
  where
    mp' = M.adjust (const high) from mp
    signal = not$foldl' (&&) True$M.elems mp'

pairPl (a,b) (c,d) = (a+c,b+d)
