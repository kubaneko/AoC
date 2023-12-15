import           Control.Applicative
import           Data.List
import           Data.List.HT
import           Data.List.Index
import           Data.List.Split
import           Debug.Trace

main :: IO ()
main = do
  file <- readFile "input"
  print $ calculate True (lines file)

type Easy = Bool -- True je Easy

calculate ez = sum . fmap
  ( fromEnum
  . liftA2 (||) bothAsc (bothAsc . fmap reverse)
  . revHead
  . transpose
  . (if ez then id else revHead)
  . fmap (fmap (read :: String -> Int) . splitOn "-")
  . splitOn ","
  )

bothAsc = and . fmap isAscending
revHead = liftA2 (:) (reverse . head) tail
