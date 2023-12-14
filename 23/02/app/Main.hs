{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Data.Map ((!), fromList)
import qualified Data.Vector as V

main :: IO ()
main = do
  i <- readFile "input"
  print$ sum.fmap (solve.words).filter (not.null).lines$ i
  print$ sum.fmap (product.hard.drop 2.words).filter (not.null).lines$ i

solve (_:n:gm) = if possible gm then read @Int $ init n else 0


easyMap = fromList [('r',12), ('g', 13), ('b', 14)]
hardMap = fromList [('r',0), ('g', 1), ('b', 2)]

possible :: [String] -> Bool
possible (n : c : gm) = (read @Int n <= (easyMap ! head c)) && possible gm
possible _ = True

hard (n:c:gm) = zipWith max (V.toList$V.imap (\ind -> \_->(if ind == i then read @Int n else 0)) e ) (hard gm)
        where
            i = (hardMap ! head c)
            e = V.fromList [0,0,0]
hard [] = [0,0,0]

