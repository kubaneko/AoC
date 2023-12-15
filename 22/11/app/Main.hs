{-# LANGUAGE TypeApplications, LambdaCase, FlexibleContexts, RecordWildCards, NamedFieldPuns #-}
import           Control.Applicative
import           Control.Composition
import           Control.Monad
import           Data.Either
import           Data.Either.Combinators        ( fromRight' )
import           Data.List
import           Data.List.Tools
-- import           Data.List.Split
import           Data.Tuple.Extra
import           Data.Void
import           Debug.Trace
import           Text.Megaparsec
import qualified Text.Megaparsec               as M
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer    as L

type Parser a = Parsec Void String a

traceI :: Show a => a -> a
traceI a = trace (show a) a

main :: IO ()
main = do
  file <- readFile "input"
  print $ calculate False file

type Easy = Bool -- True je Easy

calculate ez fl =
  product
    . take 2
    . reverse
    . sort
    . snd
    . last
    . take (if ez then 21 else 10001)
    $ iterate (calc (length monkeys) (foldl1 lcm (fmap divi monkeys)))
              (monkeys, zipWith const [0, 0 ..] monkeys)
  where monkeys = fromRight' $ parse monkeysP "input" fl

data Monkey = Monkey'
  { tru   :: Int
  , fal   :: Int
  , items :: [Int]
  , oper  :: Int -> Int
  , divi  :: Int
  , test  :: Int -> Bool
  }
ez = False

instance Show Monkey where
  show Monkey' { tru, fal, items } = show tru ++ show fal ++ show items

monkeysP :: Parser [Monkey]
monkeysP = monkeyP `sepBy` newline

monkeyP :: Parser Monkey
monkeyP = do
  items        <- takeWhileP Nothing (/= '\n') *> newline *> itemsP
  oper         <- operP
  (test, divi) <- testP
  tru          <- truP
  fal          <- falP
  return (Monkey' { .. })

truP :: Parser Int
truP = string "    If true: throw to monkey " *> L.decimal <* newline
falP :: Parser Int
falP = string "    If false: throw to monkey " *> L.decimal <* newline

operP :: Parser (Int -> Int)
operP = string "  Operation: new = " *> exprP <* newline
testP :: Parser (Int -> Bool, Int)
testP = do
  divisor <- string "  Test: divisible by " *> L.decimal <* newline
  return (\x -> x `mod` divisor == 0, divisor)
itemsP :: Parser [Int]
itemsP =
  string "  Starting items: " *> (L.decimal `sepBy` string ", ") <* newline

exprP :: Parser (Int -> Int)
exprP = do
  v1 <- varP
  space
  op <- asciiChar
  space
  v2 <- varP
  return liftA2 (if op == '*' then (*) else (+)) v1 v2

varP = do
  var1 <- observing (try (string "old"))
  if isRight var1 then return id else L.decimal >>= return . const

calc :: Int -> Int -> ([Monkey], [Int]) -> ([Monkey], [Int])
calc 0 modulo (monkeys, hotMonkeys) = (monkeys, hotMonkeys)

calc i modulo (monkeys, hotMonkeys) = calc
  (i - 1)
  modulo
  (inspect (items monkey) (monkeys, hotMonkeys))
 where
  monkey = monkeys !! (length monkeys - i)
  inspect :: [Int] -> ([Monkey], [Int]) -> ([Monkey], [Int])
  inspect [] (monkeys, hotMonkeys) =
    (modifyAt monkeys (length monkeys - i) nullItems, hotMonkeys)
    where nullItems x = x { items = [] }
  inspect (x : xs) (monkeys, hotMonkeys) = inspect
    xs
    ( modifyAt monkeys    monkeyIndex          addItem
    , modifyAt hotMonkeys (length monkeys - i) (+ 1)
    )
   where
    monkeyIndex = if test monkey newVal then tru monkey else fal monkey
    newVal      = (if ez then (`div` 3) else id) (oper monkey x) `mod` modulo
    addItem x = x { items = items x ++ [newVal] }
