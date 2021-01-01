module Main where

import Move
import Parser
import Board
import Command
import Solver
import Generator

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

-- this file contains IO functions which handle the commands like load, play etc.

load :: String -> IO ([[Char]], Int, Int)
load filename = do
    contents <- readFile filename
    let board = gridify contents
        (r,c) = getPos board '@'
        board' = changeElem board r c '-'
    return (board',r,c)

-- input a sequence of moves
inputMoveSequence :: [Move] -> Int -> IO [Move]
inputMoveSequence f count = do
    putStr ("Enter Move " ++ show (count+1) ++ " >> ")
    hFlush stdout
    command <- getLine
    let move = stringToMove command
    case (move,command,f) of
        (Nothing,"",_) -> return []
        (Nothing,_,_) -> do
            putStrLn "Invalid Move. Try Again."
            inputMoveSequence f count
        (Just Func,_,[]) -> do
            putStrLn "Function not defined. Try Again."
            inputMoveSequence f count
        (Just m,_,_) -> do
            seq <- inputMoveSequence f (count+1)
            return (m:seq)

play :: [[Char]] -> Int -> Int -> [Move] -> IO ()
play board r c f = do
    printboard (changeElem board r c '@')
    seq <- inputMoveSequence f 0
    let nr = length board
        nc = if nr == 0 then 0 else length $ head board
        (r',c',errormove,bonus) = applyMoves board nr nc r c (simplifyMoves f seq) 0 []
    printboard (changeElem board r' c' '@')
    case errormove of
        Nothing ->  if (r',c') == getPos board 't'
                    then if length bonus == length (getBonus board)
                        then putStrLn ("Congrats! You reached the target with bonus " ++ show (length bonus))
                        else putStrLn ("You reached target. But only got bonus " ++ show (length bonus))
                    else putStrLn "You didnt reach the target. Try Again"
        Just (Stop ch) -> putStrLn ("Error: Could not find Condition '" ++ [ch] ++ "'")
        Just (Start rdelta cdelta) -> let Dir d = getDir rdelta cdelta
                                      in putStrLn ("Error: Can not move " ++ d)

solve :: [[Char]] -> Int -> Int -> IO [Move]
solve board r c = do
    let (nr,nc) = getBoardSize board
        bonus = getBonus board
    return ((addLoop.removeExtraCond.condenseToDirCond.fst.getShortestTrue) (
        map
            (\(r',c') ->
                let (moves, success) = solveBoard board nr nc (r+r') (c+c') (Start r' c') bonus [(r,c,length bonus)]
                in  (Start r' c':moves, success))
            (getSafeMoves board nr nc r c 0 0 bonus [])
        ))

check :: [[Char]] -> Int -> Int -> IO Bool
check board r c = do
    let (nr,nc) = getBoardSize board
        bonus = getBonus board
    return ((snd.getShortestTrue) (
        map
            (\(r',c') ->
                let (moves, success) = solveBoard board nr nc (r+r') (c+c') (Start r' c') bonus [(r,c,length bonus)]
                in  (Start r' c':moves, success))
            (getSafeMoves board nr nc r c 0 0 bonus [])
        ))

hint :: [[Char]] -> Int -> Int -> Int -> IO [Move]
hint board r c hintcount = do
    let (nr,nc) = getBoardSize board
        bonus = getBonus board
        solution = (removeExtraCond.condenseToDirCond.fst.getShortestTrue) (
            map
                (\(r',c') ->
                    let (moves, success) = solveBoard board nr nc (r+r') (c+c') (Start r' c') bonus [(r,c,length bonus)]
                    in  (Start r' c':moves, success))
                (getSafeMoves board nr nc r c 0 0 bonus [])
            )
    return (take hintcount solution)

generate :: IO ([[Char]],Int,Int)
generate = do
    let nr = 20
        nc = 30
        board = [['*' | x<-[1..30]] | y<-[1..20]]
        r = 10
        c = 0
    path <- generatePath 35
    let (bs,tr,tc) = scrape board nr nc r c path
    if tc == nc-1
        then do
            let blockfree = not $ hasBlock bs
            if blockfree
                then do
                    bwc <- addCond bs
                    bwb <- addBonus bwc
                    solvable <- check bwb r c
                    if solvable
                        then return (bwb,r,c)
                        else generate
                else generate
        else generate

-- run the command line interface for the game
run :: Int -> [[Char]] -> Int -> Int -> IO ()
run step board r c = do
    putStr ">> "
    hFlush stdout
    line <- getLine
    let command = stringToCommand line
    case (command,step) of
        (Nothing,_) -> do
            putStrLn "Invalid Command. Try Again."
            run step board r c
        (Just Quit,_) -> do
            putStrLn "Quitting..."
            return ()
        (Just (Load filename),_) -> do
            (board',r',c') <- load filename
            printboard (changeElem board' r' c' '@')
            putStrLn "Board Loaded."
            run 1 board' r' c'
        (Just Generate,_) -> do
            (board',r',c') <- generate
            printboard (changeElem board' r' c' '@')
            putStrLn "Board Generated And Checked. Play"
            run 2 board' r' c'
        (Just Check, 0) -> do
            putStrLn "No Board Loaded. Load Board First"
            run step board r c
        (Just Check, _) -> do
            checked <- check board r c
            if checked
                then do
                    putStrLn "Board Valid. Can Reach Target. Play"
                    run 2 board r c
                else do
                    putStrLn "Board Invalid. Load Another One."
                    run step board r c
        (Just Solve, 2) -> do
            solution <- solve board r c
            dispMoves solution
            run step board r c
        (Just Solve, _) -> do
            putStrLn "Cannot Solve Before Loading And Checking."
            run step board r c
        (Just Play, 2) -> do
            play board r c []
            run step board r c
        (Just Play, _) -> do
            putStrLn "Cannot Play Before Loading And Checking"
            run step board r c
        (Just (PlayF m1 m2 m3), 2) -> do
            play board r c [m1,m2,m3]
            run step board r c
        (Just (PlayF _ _ _),_) -> do
            putStrLn "Cannot Play Before Loading and Checking"
            run step board r c
        (Just (Hint hcount), 2) -> do
            hints <- hint board r c hcount
            dispMoves hints
            run step board r c
        (Just (Hint _),_) -> do
            putStrLn "Cannot Take Hint Before Loading and Checking"
            run step board r c

-- main
main :: IO ()
main = run 0 [[]] 0 0
