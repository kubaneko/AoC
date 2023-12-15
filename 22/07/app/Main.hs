import Control.Applicative
import Control.Monad
import Data.Bifunctor
import Data.Either
import Data.Either.Extra
import Data.GI.Base.Utils
import Data.List
import Data.List.Split
import Data.Void
import Debug.Trace
import Distribution.Simple.Utils
import Safe

traceI :: Show a => a -> a
traceI a = trace (show a) a

main :: IO ()
main = do
    file <- readFile "input"
    print $ calculate False file --lines

type Easy = Bool -- True je Easy

calculate ez fl =
    ( if ez
        then sum . filter (<= 100000)
        else findJust (>= (rootSize - 40000000) . sort)
    )
        $ fmap (sum . dirSize parse . fst) parse
  where
    rootSize = sum $ dirSize parse ["/"]
    lexed = fmap (fmap (splitOn " ") . lines) (drop 1 $ splitOn "$ " fl)
    parse = nub $ filter (not . null . snd) $ foldl fdl [] lexed
    fdl ((dr, fls) : xs) command
        | (safeHead command >>= safeHead)
            == Just "cd"
            && (safeHead command >>= safeHead . safeTail)
            == Just ".." =
            (drop 1 dr, []) : (dr, fls) : xs
        | (safeHead command >>= safeHead) == Just "cd" && null fls =
            (head (tail $ head command) : dr, []) : xs
        | (safeHead command >>= safeHead) == Just "cd" =
            (head (tail $ head command) : dr, []) : (dr, fls) : xs
        | (safeHead command >>= safeHead) == Just "ls" =
            (dr, parseEntry dr <$> safeTail command) : xs
    fdl [] command
        | (safeHead command >>= safeHead) == Just "cd" =
            [([head $ tail $ head command], [])]
    parseEntry path (size : name : brb)
        | size == "dir" = Right (name : path)
        | otherwise = Left (read size :: Int, name)

dirSize graph path =
    concatMap fromEither $
        bimap ((: []) . fst) (dirSize graph)
            <$> lookupJust path graph
