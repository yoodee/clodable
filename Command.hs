module Command where

import Move
import Board
import Parser
import Generator
import Solver

import System.IO
import Control.Applicative hiding (optional)
import Control.Monad
import Control.Monad.State
import Prelude

import Data.Char
import Data.Functor
import Data.List
import Data.Monoid

import Text.Printf

data Command =
    Load String |
    Play |
    PlayF Move Move Move |
    Check |
    Solve |
    Hint Int |
    Generate |
    Quit
    deriving (Show)

-- convert String to Command
stringToCommand :: String -> Maybe Command
stringToCommand s = case runParser comP s of
    [] -> Nothing
    [(v,out)] -> Just v

-- load parser
loadP :: Parser Command
loadP = do
    string "load"
    some (char ' ')
    char '"'
    filename <- some next
    char '"'
    return (Load filename)

-- generate parser
generateP :: Parser Command
generateP = do
    string "generate"
    return Generate

-- play parser
playP :: Parser Command
playP = do
    string "play"
    return Play

-- play with functions parser
playFP :: Parser Command
playFP = do
    string "play"
    some (char ' ')
    m1 <- condP +++ dirP
    some (char ' ')
    m2 <- condP +++ dirP
    some (char ' ')
    m3 <- condP +++ dirP
    return (PlayF m1 m2 m3)

-- check parser
checkP :: Parser Command
checkP = do
    string "check"
    return Check

-- solve parser
solveP :: Parser Command
solveP = do
    string "solve"
    return Solve

-- hint parser
hintP :: Parser Command
hintP = do
    string "hint"
    some (char ' ')
    c <- some (satisfy isNumber)
    let cint = read c :: Int
    return (Hint cint)

-- quit parser
quitP :: Parser Command
quitP = do
    string "quit"
    return Quit

-- command parser
comP :: Parser Command
comP = do
    many (char ' ')
    c <- loadP +++ generateP +++ playP +++ playFP +++ checkP +++ solveP +++ hintP +++ quitP
    many (char ' ')
    eof
    return c
