{-# language MultiWayIf#-}

module Main where

import Data.Char
import Data.List
import Debug.Trace
import qualified Data.Vector as V

data Diff = H | E
  deriving (Eq)

diff = H

main :: IO ()
main = do
  i <- readFile "input"
  parse <- return$fmap ((\[hand,bet]->(fmap conv hand, read @Int bet)).words)$lines i
  print$sum$V.toList$V.imap (\i -> \case (_,b)->(i+1)*b)$V.fromList$sortBy
    (\a b -> compareHands (fst a) (fst b)) parse
  where
    conv a
        | isDigit a = digitToInt a
        | a == 'T' = 10
        | a == 'J' = if diff == H then 0 else 11
        | a == 'Q' = 12
        | a == 'K' = 13
        | a == 'A' = 14

compareHands h1 h2 = if | sh1 <= sh2 && sh2 <= sh1 -> compare h1 h2
                        | sh1 < sh2 -> LT
                        | otherwise -> GT
  where
    gh1 = diffMod (group$sort h1) diff
    gh2 = diffMod (group$sort h2) diff
    sh1 = reverse.sort$fmap length gh1
    sh2 = reverse.sort$fmap length gh2
    diffMod x E = x
    diffMod [k] _ = [k]
    diffMod ((zrs@(0:_):xs)) _ = init xs' ++ [last xs' ++ zrs]
      where
        xs' = sortOn length xs
    diffMod k _ = k
