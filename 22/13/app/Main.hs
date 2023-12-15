{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

import Control.Applicative
import Control.Composition
import Control.Monad
import Data.Either
import Data.Either.Combinators (fromRight')
import Data.List
import Data.List.Index
import Data.List.Tools

-- import           Data.List.Split
import Data.Tuple.Extra
import Data.Void
import Debug.Trace
import Safe
import Text.Megaparsec
import qualified Text.Megaparsec as M
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser a = Parsec Void String a

traceI :: Show a => a -> a
traceI a = trace (show a) a

main :: IO ()
main = do
    file <- readFile "input"
    print $ calculate file

ez = False

calculate fl =
    ( if ez
        then
            show
                . sum
                . imap
                    ( \i ->
                        ( \case
                            True -> (i + 1)
                            False -> 0
                        )
                    )
                . fmap (uncurry (<=))
        else
            show
                . liftA2
                    (*)
                    ((+ 1) . elemIndexJust packet1)
                    ((+ 1) . elemIndexJust packet2)
                . sort
                . ([packet1, packet2] ++)
                . concatMap (\(a, b) -> [a, b])
    )
        ((fromRight' $ parse inputP "" fl) :: [(Signals, Signals)])

packet1 = Signals' [Signals' [Packet 6]]
packet2 = Signals' [Signals' [Packet 2]]

data Signals = Packet Int | Signals' [Signals]
    deriving (Show)

instance Eq Signals where
    (==) (Packet a) (Packet b) = a == b
    (==) (Packet a) (Signals' b) = [Packet a] == b
    (==) (Signals' a) (Packet b) = a == [Packet b]
    (==) (Signals' a) (Signals' b) = a == b

instance Ord Signals where
    (<=) (Packet a) (Packet b) = a <= b
    (<=) (Packet a) (Signals' b) = [Packet a] <= b
    (<=) (Signals' a) (Packet b) = a <= [Packet b]
    (<=) (Signals' a) (Signals' b) = a <= b

signalP :: Parser Signals
signalP = try packetP <|> signalsP

packetP = Packet <$> L.decimal
signalsP =
    Signals' <$> (string "[" *> signalP `sepBy` string "," <* string "]")

inputP :: Parser [(Signals, Signals)]
inputP = liftA2 (,) (signalP <* newline) (signalP <* newline) `sepBy` newline
