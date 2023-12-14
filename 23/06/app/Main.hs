module Main where

import Debug.Trace
import Data.List
import Data.List.Split (split, oneOf)

data Diff = H | E
  deriving (Eq)


main :: IO ()
main = readFile "input" >>= print.product.fmap (\case [t,r] -> length$filter id$fmap (\k -> k*(t-k)>r) [0..t]).transpose.fmap (fmap (read @Integer).(if h == H then (: []).concat else id).drop 1.words).lines
  where
        h = H
