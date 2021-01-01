module Generator where

import System.Random
import Board
import Solver

-- add up to 3 bonuses to a board
addBonus :: [[Char]] -> IO [[Char]]
addBonus board = do
    let (nr,nc) = getBoardSize board
        bps = getBP board nr nc
    g <- newStdGen
    let [(r1,c1),(r2,c2),(r3,c3)] = map (bps !!) $ take 3 (randomRs (0,length bps-1) g)
        b1 = changeElem board r1 c1 'b'
        b2 = changeElem b1 r2 c2 'b'
        b3 = changeElem b2 r3 c3 'b'
    return b3

-- get all positions of '-' 
getBP :: [[Char]] -> Int -> Int -> [(Int,Int)]
getBP board nr nc = filter (\(r,c) -> board!!r!!c=='-' && (r,c)/=(10,0)) [(r,c) | r <- [0..nr-1], c<-[0..nc-1]]

-- add condition to the T points of a generated map
addCond :: [[Char]] -> IO [[Char]]
addCond board = do
    let (nr,nc) = getBoardSize board
        tps = getTP board nr nc
    modifyTP board tps

-- modify T points
modifyTP :: [[Char]] -> [(Int,Int)] -> IO [[Char]]
modifyTP board [] = return board
modifyTP board ((r,c):xs) = do
    g <- newStdGen
    let ch = "pyo"!!head (randomRs (0,2) g)
    modifyTP (changeElem board r c ch) xs

-- find all T points
getTP :: [[Char]] -> Int -> Int -> [(Int,Int)]
getTP board nr nc =
    filter
        (\(r,c) -> length (getSafeMoves board nr nc r c 0 0 [] []) >= 3)
        (getBP board nr nc)

-- find the existence of block of '-' that are 2x2 or greater
hasBlockRows :: [Char] -> [Char] -> Bool
hasBlockRows [] _ = False
hasBlockRows _ [] = False
hasBlockRows [x] _ = False
hasBlockRows _ [x] = False
hasBlockRows (x1:x2:xs) (y1:y2:ys) = all (=='-') [x1,x2,y1,y2] || hasBlockRows (x2:xs) (y2:ys)

-- find a block of '-' in a pair of rows
hasBlock :: [[Char]] -> Bool
hasBlock [] = False
hasBlock [x] = False
hasBlock (x1:x2:xs) = hasBlockRows x1 x2 || hasBlock (x2:xs)

-- change '*' to '-'
scrape :: [[Char]] -> Int -> Int -> Int -> Int -> [(Int,Int)] -> ([[Char]],Int,Int)
scrape board nr nc r c [] = (changeElem board r c 't',r,c)
scrape board nr nc r c ((rdelta,cdelta):s) =
    if (r+rdelta) `elem` [0..nr-1] && (c+cdelta) `elem` [0..nc-1]
        then scrape board' nr nc (r+rdelta) (c+cdelta) s
        else scrape board' nr nc r c s
  where
      board' = changeElem board r c '-'

-- generate a random path
generatePath :: Int -> IO [(Int,Int)]
generatePath 0 = return [(0,1) | x<-[0..7] ]
generatePath n = do
    g <- newStdGen
    let i = head $ randomRs (0::Int,3) g
    let move = [(0,1),(1,0),(0,-1),(-1,0)]!!i
    let count = head $ randomRs (5::Int,7) g
    let moves = [move | x<-[1..count]]
    nextmoves <- generatePath (n-1)
    return (nextmoves ++ moves)
