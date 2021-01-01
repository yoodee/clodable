module Solver where

import Move
import Parser
import Board

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

-- get a list of safe moves which do not result in a position already visited
getSafeMoves :: [[Char]] -> Int -> Int -> Int -> Int -> Int -> Int -> [(Int,Int)] -> [(Int,Int,Int)] -> [(Int,Int)]
getSafeMoves board nr nc r c _ _ bonus stopsdb =
    filter
        (\(r',c') -> ((r+r',c+c',length $ delete (r,c) bonus) `notElem` stopsdb) && not (isUnsafe board nr nc (r+r') (c+c')))
        [(0,1),(1,0),(0,-1),(-1,0)]

-- get the shortest of all paths which reach the target
getShortestTrue :: [([SimpleMove],Bool)] -> ([SimpleMove],Bool)
getShortestTrue [] = ([],False)
getShortestTrue (x:xs) =
    case (x, getShortestTrue xs) of
        ((_,False),(_,False)) -> ([],False)
        ((_,False),(v,True))  -> (v,True)
        ((u,True),(_,False))  -> (u,True)
        ((u,True),(v,True))   -> if countTurns u < countTurns v then (u,True) else (v,True)
  where
    countTurns :: [SimpleMove] -> Int
    countTurns [] = 0
    countTurns [Start _ _] = 0
    countTurns (Stop _:xs) = countTurns xs
    countTurns (Start r1 c1:Start r2 c2:xs)
      | (r1,c1)==(r2,c2) = countTurns (Start r2 c2:xs)
      | otherwise        = 1+countTurns (Start r2 c2:xs)
    countTurns (Start r1 c1:Stop ch:Start r2 c2:xs)
      | (r1,c1)==(r2,c2) = countTurns (Start r2 c2:xs)
      | otherwise        = 1+countTurns (Start r2 c2:xs)

-- solve the board and return a path and whether the target is reachable with all bonuses
solveBoard :: [[Char]] -> Int -> Int -> Int -> Int -> SimpleMove -> [(Int,Int)] -> [(Int,Int,Int)] -> ([SimpleMove],Bool)
solveBoard board nr nc r c (Start rdelta cdelta) bonus stopsdb
  | board!!r!!c == 't' && null bonus && isUnsafe board nr nc (r+rdelta) (c+cdelta) = ([],True)
  | (r+rdelta,c+cdelta,length $ delete (r,c) bonus) `elem` stopsdb = ([],False)
  | isUnsafe board nr nc (r+rdelta) (c+cdelta) = getShortestTrue (
      map
        (\(r',c') -> 
          let (moves,success) = solveBoard board nr nc (r+r') (c+c') (Start r' c') (delete (r,c) bonus) ((r,c,length $ delete (r,c) bonus):stopsdb)
          in (Start r' c':moves, success))
        (getSafeMoves board nr nc r c rdelta cdelta bonus stopsdb)
    )
  | board!!r!!c `elem` "pyo" = getShortestTrue (
      map
        (\(r',c') ->
            let (moves, success) = solveBoard board nr nc (r+r') (c+c') (Start r' c') (delete (r,c) bonus) ((r,c,length $ delete (r,c) bonus):stopsdb)
            in  (Stop (board!!r!!c):Start r' c':moves, success))
        (getSafeMoves board nr nc r c rdelta cdelta bonus stopsdb)
    )
  | otherwise = solveBoard board nr nc (r+rdelta) (c+cdelta) (Start rdelta cdelta) (delete (r,c) bonus) stopsdb