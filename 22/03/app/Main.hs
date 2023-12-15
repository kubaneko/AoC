import           Control.Conditional
import "monad-extras" Control.Monad.Extra
import           Data.Char
import           Data.List.Extra
import           Debug.Trace

main :: IO ()
main = do
  path <- getLine
  file <- readFile path
  print $ calculate False (lines file)

type Easy=Bool -- True je Easy

sfork p f a = p (f a) a

calculate :: Easy -> [String] -> Int
calculate ez lns =
  sumOn' head
    $ ( fmap
          (fmap
            ((+) (1 - ord 'a') . select isUpper ((+) 26 . ord . toLower) ord)
          )
      . if ez then 
          (fmap (assocFoldl1 intersect . genChunks))
        else
          (fmap (assocFoldl1 intersect) . genChunks)
      )
        lns
        where
            genChunks=sfork chunksOf (if ez flip div 2.length else const 3)
