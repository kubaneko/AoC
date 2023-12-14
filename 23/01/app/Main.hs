{-# LANGUAGE TypeApplications #-}

module Main where

import Data.Char
import Data.List

data Diff = E | H
  deriving (Eq)

main :: IO ()
main = do
  i <- readFile "input"
  if dif == E then print @Int $ sum $ fmap ((\k -> read $ head k : [last k]).filter isDigit) $lines i
    else print$sum$ fmap ((\k -> read @Int $ head k : [last k]).filter isDigit.(\k -> fun2 isPrefixOf tail head k ++ k ++ fun2 isSuffixOf init last k)) $ filter (not.null)$ lines i
    where
      dif = H


fun2 f g h s = if isDigit (h s) then "" else if not$null$ fun f s then fun f s else fun2 f g h (g s)

fun f s = if f "one" s then "1" else if  f "two" s then "2" else if f "three" s
        then "3" else if f "four" s then "4" else if  f "five" s then "5" else if  f
        "six" s then "6" else if f "seven" s then "7" else if f "eight" s then "8" else
        if f "nine" s then "9" else ""
