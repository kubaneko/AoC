{-# LANGUAGE TypeApplications #-}
import           Control.Applicative
import           Data.List
import           Data.List.Split
import           Data.Tuple.Extra
import           Debug.Trace

traceI :: Show a => a -> a
traceI a = trace (show a) a

main :: IO ()
main = do
  file <- readFile "input"
  print $ calculate False (lines file)

type Easy = Bool -- True je Easy

calculate ez lns =
  length $ nub $ fmap (!! (len - 1)) $ foldl' fld [replicate len (0, 0)] $ parse
    lns
  where len = if ez then 2 else 10

parse :: [String] -> [(String, Int)]
parse = fmap ((\(a : b : bs) -> (a, read @Int b)) . splitOn " ")

fld (x : xs) (dir, k) = (++ xs) $ reverse $ take (k + 1) $ iterate
  (update diff)
  x
 where
  diff = case dir of
    "D" -> (0, -1)
    "U" -> (0, 1)
    "R" -> (1, 0)
    "L" -> (-1, 0)

update :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
update diff (h : rs) = reverse $ foldl'
  (\(lst : rest) next ->
    (if vabs (vmin lst next) <= 1
        then next
        else vplu next (both signum (vmin lst next))
      )
      : (lst : rest)
  )
  [newH]
  rs
  where newH = vplu h diff

vmin (a, b) (c, d) = (a - c, b - d)
vplu (a, b) (c, d) = (a + c, b + d)
vabs (a, b) = max (abs a) (abs b)
