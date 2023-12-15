{-# LANGUAGE LambdaCase, FlexibleContexts #-}
import           Control.Applicative
import           Data.Char
import           Data.Graph.Inductive.Basic
import           Data.Graph.Inductive.Graph
import           Data.Graph.Inductive.Query.BFS
import           Data.Graph.Inductive.Tree
import           Data.List
import           Data.List.Index
import           Data.List.Split
import           Data.List.Tools
import           Data.Maybe
import           Data.Tuple
import           Data.Tuple.Extra
import           Debug.Trace
import           Safe

traceI :: Show a => a -> a
traceI a = trace (show a) a

main :: IO ()
main = do
  file <- readFile "input"
  print $ calculate (lines file)

ez = False

calculate lns =
  flip (-) 1
    $   length
    $   head
    $   sortOn length
    $   filter (not . null)
    $   (\a -> esp a end graph)
    <$> starts
 where
  starts :: [Node]
  starts = if ez
    then
      [ toNode . liftA2 (,) (head . catMaybes) (findIndexJust isJust) $ fmap
          (elemIndex 'S')
          lns
      ]
    else
      toNode
        <$> (catMaybes . concat . imap
              (\y ->
                imap
                  (\x a -> if a == 'S' || a == 'a' then Just (x, y) else Nothing
                  )
              )
            )
              lns
  end :: Node
  end = toNode . liftA2 (,) (head . catMaybes) (findIndexJust isJust) $ fmap
    (elemIndex 'E')
    lns
  toNode (a, b) = a + b * sizeX
  sizeX = length (head heights)
  lns' :: [[Char]]
  lns' = fmap
    (fmap
      (\case
        'S' -> 'a'
        'E' -> 'z'
        a   -> a
      )
    )
    lns
  heights   = fmap (fmap (flip (-) (ord 'a') . ord)) lns'
  leftEdges = generateEdges 1 sizeX
    $ zipWith (zipWith (flip (-))) heights (fmap (drop 1) heights)
  upEdges = generateEdges sizeX sizeX
    $ zipWith (zipWith (flip (-))) heights (drop 1 heights)
  allEdges :: [Edge]
  allEdges = upEdges ++ leftEdges
  nodes :: [Node]
  nodes = take (sizeX * length heights) [0 ..]
  graph :: Gr () ()
  graph = mkUGraph nodes allEdges

generateEdges diff size = concat . concat . imap
  (\y -> imap
    (\x diff -> if abs diff <= 1
      then [edge x y, swap (edge x y)]
      else (if diff < 0 then [edge x y] else [swap (edge x y)])
    )
  )
  where edge x y = (size * y + x, size * y + x + diff)
