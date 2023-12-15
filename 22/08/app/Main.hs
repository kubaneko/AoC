import Configuration.Utils.Operators
import Control.Applicative
import Control.Lens.Lens
import Control.Monad
import Data.Bifunctor
import Data.Char
import Data.Either
import Data.Either.Extra
import Data.Function
import Data.List
import Data.List.Split
import Debug.Trace
import Safe

traceI :: Show a => a -> a
traceI a = trace (show a) a

main :: IO ()
main = do
    file <- readFile "input"
    print $ calculate False (lines file)

type Easy = Bool -- True je Easy

calculate ez lns =
    if ez
        then
            sum $
                fmap
                    (sum . fmap fromEnum)
                    ( foldl1 (zipWith (zipWith (||))) $
                        withDirections (fmap (markReached (-1))) parse
                    )
        else
            maximum . fmap maximum . foldl1 (zipWith (zipWith (*))) $
                withDirections
                    viewsMatrix
                    parse
  where
    parse = fmap (fmap digitToInt) lns
    markReached min =
        reverse . snd . foldl' (if ez then markEasy else markHard) (min, [])
    markEasy (max, list) a =
        if a > max then (a, True : list) else (max, False : list)
    markHard (max, list) a
        | max == (minBound :: Int) = (max, False : list)
        | a < max = (max, True : list)
        | otherwise = ((minBound :: Int), True : list)
    viewsMatrix =
        reverse
            . fmap reverse
            . foldl'
                ( \mat row ->
                    snd
                        ( foldl'
                            ( \(k, ls) b ->
                                ( k + 1
                                , (sum . fmap fromEnum . markReached (b) $ drop k (row)) :
                                  ls
                                )
                            )
                            (1, [])
                            (row)
                        ) :
                    mat
                )
                []

    withDirections f a =
        zipWith
            ($)
            (transp ([id, fmap reverse] >*> [id, transpose]))
            (f <$> (([id, fmap reverse] <*< [id, transpose]) ?? a))

transp (a : b : c : d) = a : c : b : d
