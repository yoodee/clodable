module Move where

import Parser

import Control.Applicative hiding (optional)
import Control.Monad
import Control.Monad.State
import Prelude

import Data.Char
import Data.Functor
import Data.List
import Data.Monoid

import Text.Printf

-- data type for Simple Move : Start rdelta cdelta or Stop ch 
data SimpleMove =
    Start Int Int |
    Stop Char
        deriving (Show,Eq,Ord)

-- data type for Move
data Move =
    Dir String |
    Cond Char Move |
    Loop Int Move Move |
    Func
        deriving (Eq,Ord)

instance Show Move where
    -- show :: Move -> String
    show (Dir d) = d
    show (Cond t d) = "Cond{"++[t]++"}{"++(show d)++"}"
    show (Loop c m1 m2) = "Loop{"++(show c)++"}{"++(show m1)++","++(show m2)++"}"
    show Func = "Function"

-- direction parser
dirP :: Parser Move
dirP = do
    d <- string "Right" +++
         string "Left"  +++
         string "Up"    +++
         string "Down"
    return (Dir d)

-- condition parser
condP :: Parser Move
condP = do
    string "Cond{"
    t <- char 'o' +++ char 'y' +++ char 'p'
    string "}{"
    d <- dirP
    string "}"
    return (Cond t d)

-- loop parser
loopP :: Parser Move
loopP = do
    string "Loop{"
    c <- satisfy (`elem` "012345")
    let cint = read [c] :: Int
    string "}{"
    m1 <- dirP +++ condP
    string ","
    m2 <- dirP +++ condP
    string "}"
    return (Loop cint m1 m2)

-- function parser
funcP :: Parser Move
funcP = do
    string "Function"
    return Func

-- move parser
moveP :: Parser Move
moveP = do
    m <- dirP +++ condP +++ loopP +++ funcP
    eof
    return m

-- convert string to move
stringToMove :: String -> Maybe Move
stringToMove s = case runParser moveP s of
    [] -> Nothing
    [(v,out)] -> Just v

-- convert direction to Start rdelta cdelta
getStart :: String -> SimpleMove
getStart "Left" = Start 0 (-1)
getStart "Right" = Start 0 1
getStart "Up" = Start (-1) 0
getStart "Down" = Start 1 0

-- convert rdelta cdelta to direction
getDir :: Int -> Int -> Move
getDir 0 (-1) = Dir "Left"
getDir 0 1 = Dir "Right"
getDir (-1) 0 = Dir "Up"
getDir 1 0 = Dir "Down"

-- print a list of moves
dispMoves :: [Move] -> IO ()
dispMoves [] = putStrLn ""
dispMoves [x] = print x
dispMoves (x:xs) = do
    putStr (show x ++ " ")
    dispMoves xs

-- convert moves to simplemoves
simplifyMoves :: [Move] -> [Move] -> [SimpleMove]
simplifyMoves f [] = []
simplifyMoves f (Func:s) = simplifyMoves f f ++ simplifyMoves f s
simplifyMoves f (Loop 0 m1 m2: s) = simplifyMoves f s
simplifyMoves f (Loop c m1 m2: s) = simplifyMoves f [m1,m2] ++ simplifyMoves f (Loop (c-1) m1 m2: s)
simplifyMoves f (Cond t (Dir d):s) = [Stop t, getStart d] ++ simplifyMoves f s
simplifyMoves f (Dir d:s) = getStart d:simplifyMoves f s

-- convert simplemoves to moves 
condenseToDirCond :: [SimpleMove] -> [Move]
condenseToDirCond [] = []
condenseToDirCond (Stop ch:Start r c:xs) = Cond ch (getDir r c):condenseToDirCond xs
condenseToDirCond (Start r c:xs) = getDir r c:condenseToDirCond xs

-- get next turn in a list of moves
getNextTurn :: Int -> [Move] -> Move -> (Int,Maybe Char)
getNextTurn count [] lm = (count,Nothing)
getNextTurn count (Cond ch d:ms) lm = if d /= lm then (count,Just ch) else getNextTurn (count+1) ms lm
getNextTurn count (d:ms) lm = if d /= lm then (count,Nothing) else getNextTurn (count+1) ms lm

-- removes extra Conditionals
removeExtraCond :: [Move] -> [Move]
removeExtraCond [] = []
removeExtraCond (Cond ch d:ms) = case getNextTurn 0 ms d of
    (count,Nothing) -> Cond ch d : (removeExtraCond.drop count) ms
    (count,Just c)  -> Cond ch d : (filter (\(Cond c' d') -> c==c') . take count) ms ++ (removeExtraCond.drop count) ms
removeExtraCond (d:ms) = case getNextTurn 0 ms d of
    (count,Nothing) -> d : (removeExtraCond.drop count) ms
    (count,Just c)  -> d : (filter (\(Cond c' d') -> c==c') . take count) ms ++ (removeExtraCond.drop count) ms

-- Add loops to a list of moves
addLoop :: [Move] -> [Move]
addLoop [] = []
addLoop [m1] = [m1]
addLoop [m1,m2] = [m1,m2]
addLoop (Loop n m1 m2:m3:m4:ms) = if (m1,m2) == (m3,m4) then addLoop (Loop (n+1) m1 m2:ms) else Loop n m1 m2:addLoop (m3:m4:ms)
addLoop [m1,m2,m3] = [m1,m2,m3]
addLoop (m1:m2:m3:m4:ms) = if (m1,m2) == (m3,m4) then addLoop (Loop 2 m1 m2:ms) else m1:addLoop (m2:m3:m4:ms)
