module Board where

import Move
import Parser

import Control.Applicative hiding (optional)
import Control.Monad
import Control.Monad.State
import Prelude

import Data.Char
import Data.Functor
import Data.List
import Data.Monoid
import Data.Maybe

import Text.Printf

-- returns the size of the board passed
getBoardSize :: [[Char]] -> (Int,Int)
getBoardSize board =
    let nr = length board
        nc = if nr == 0 then 0 else length $ head board
    in (nr,nc)

-- changes elements in a row
changeElemRow :: [a] -> Int -> a -> [a]
changeElemRow row c ch =
    let (left, _:right) = splitAt c row
    in left ++ [ch] ++ right

-- modify elements in the board
changeElem :: [[a]] -> Int -> Int -> a -> [[a]]
changeElem board r c ch =
    let row = board!!r
        new_row = changeElemRow row c ch
    in changeElemRow board r new_row

-- get position of a particular element in the board
getPos :: [[Char]] -> Char -> (Int,Int)
getPos board ch = let r = fromMaybe (-1) $ findIndex (elem ch) board
                      c = if r==(-1) then -1 else fromMaybe (-1) $ elemIndex ch (board!!r)
                  in (r,c)

-- check if a particular position is unsafe
isUnsafe :: [[Char]] -> Int -> Int -> Int -> Int -> Bool
isUnsafe board nr nc r c = r >= nr || r < 0 || c >= nc || c < 0 || board!!r!!c=='*'

-- get a list of positions of all bonuses
getBonus :: [[Char]] -> [(Int,Int)]
getBonus board
  | (r,c)==(-1,-1) = []
  | otherwise      = (r,c) : getBonus (changeElem board r c '-')
  where
    (r,c) = getPos board 'b'

-- update the list of bonuses collected
updateBonus :: [[Char]] -> Int -> Int -> [(Int,Int)] -> [(Int,Int)]
updateBonus board r c bonus  =
    if board!!r!!c == 'b' && (r,c) `notElem` bonus
    then (r,c):bonus
    else bonus

-- apply a list of SimpleMoves to the board given the board, its size and the ball's current position
applyMoves :: [[Char]] -> Int -> Int -> Int -> Int -> [SimpleMove] -> Int -> [(Int,Int)] -> (Int,Int,Maybe SimpleMove,[(Int,Int)])
applyMoves board nr nc r c [] _ bonus = (r,c,Nothing,newbonus)
  where
    newbonus = updateBonus board r c bonus
applyMoves board nr nc r c (Start rdelta cdelta:s) 0 bonus
  | isUnsafe board nr nc (r+rdelta) (c+cdelta) = (r,c,Just (Start rdelta cdelta),newbonus)
  | otherwise = applyMoves board nr nc (r+rdelta) (c+cdelta) (Start rdelta cdelta:s) 1 newbonus
  where
    newbonus = updateBonus board r c bonus
applyMoves board nr nc r c (Start rdelta cdelta:Stop ch:s) count bonus
  | board!!r!!c == ch = applyMoves board nr nc r c s 0 newbonus
  | isUnsafe board nr nc (r+rdelta) (c+cdelta) = (r,c,Just (Stop ch), newbonus)
  | otherwise = applyMoves board nr nc (r+rdelta) (c+cdelta) (Start rdelta cdelta:Stop ch:s) (count+1) newbonus
  where
    newbonus = updateBonus board r c bonus
applyMoves board nr nc r c (Start rdelta cdelta:s) count bonus
  | isUnsafe board nr nc (r+rdelta) (c+cdelta) = applyMoves board nr nc r c s 0 newbonus
  | otherwise = applyMoves board nr nc (r+rdelta) (c+cdelta) (Start rdelta cdelta:s) (count+1) newbonus
  where
    newbonus = updateBonus board r c bonus
applyMoves board nr nc r c (Stop ch: s) _ bonus = (r,c,Just (Stop ch),newbonus)
  where
    newbonus = updateBonus board r c bonus

-- input the board as a string and convert it into 2d list
gridify :: String -> [[Char]]
gridify [] = []
gridify ('\n':board) = gridify board
gridify board =
    filter (/=' ') (takeWhile (/='\n') board) :
    gridify (dropWhile (/='\n') board)

-- print the board
printboard :: [[Char]] -> IO ()
printboard [] = return ()
printboard b = do
    let row = head b
    putStrLn (intersperse ' ' row)
    printboard (tail b)

