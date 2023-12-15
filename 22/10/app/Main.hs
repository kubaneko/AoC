{-# LANGUAGE TypeApplications, LambdaCase, FlexibleContexts #-}
import           Control.Applicative
import           Data.List
import           Data.List.Tools
import           Data.List.Split
import           Data.Tuple.Extra
import           Debug.Trace

traceI :: Show a => a -> a
traceI a = trace (show a) a

main :: IO ()
main = do
  file <- readFile "input"
  putStr $ calculate False (lines file)

type Easy = Bool -- True je Easy

calculate ez lns = if ez then show.sum.snd3$foldl' calc (1,[],0) calendar else reverse.snd3$foldl' calcH (1,[],-1) calendar
    where 
        parse = Nothing:(fmap ((\case {(x:y:xs)-> Just (read @Int y) ; _-> Nothing }) . splitOn " ") lns)
        calendar=(\(i,cal)->take (i+2) cal)$foldl' fld (0,(0:[0,0..])) parse
        fld (ind,cal) Nothing=(ind+1,cal)
        fld (ind,cal) (Just a)=(ind+2, modifyAt cal (ind+2) (+a))
        calc (reg,tot,ind) diff= (reg+diff,(if (ind `mod` 40)==20 then ((ind*(reg+diff)):tot) else tot), ind+1)

        calcH (reg,tot,ind) diff= trace (show (reg,tot,ind, diff)) (reg+diff, newBrb reg tot ind diff, ind+1)
        newBrb reg xs ind diff
            | ind `mod` 40==0 = ((update (reg+diff) ind):'\n':xs)
            | otherwise = (update (reg+diff) ind):xs
        update reg ind=if abs (reg-(ind `mod` 40))<2 then '#' else '.'

