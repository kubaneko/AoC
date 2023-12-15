{-# OverloadedStrings #-}
import           Control.Applicative
import           Data.List
import           Data.List.Extra
import           Data.List.Split
import Text.Megaparsec.Char
import Text.Megaparsec
import qualified Text.Megaparsec.Char.Lexer as L
import           Debug.Trace
import Data.Text
import Data.Void

data Entry = Dir' Text | File Int Text
type Dir=Text
type FileSys=[(Dir, [Entry])]

traceI :: Show a => a -> a
traceI a = trace (show a) a

main :: IO ()
main = do
  file <- readFile "input"
  print $ calculate False file --lines

type Easy = Bool -- True je Easy

calculate ez fl = "sadf"

withNewLines = flip sepEndBy newline

parseDir :: CharParser Dir
parseDir = "dir" *> spaceChar *> eof 

type CharParser a = Parsec Void String a
