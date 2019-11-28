{-# LANGUAGE OverloadedStrings #-}

import System.IO
import System.Environment (getArgs)
import Data.Char (chr)

main :: IO()
main = do
    args <- getArgs
    withFile (args !! 0) ReadMode $ \handle -> do
        program <- hGetContents handle
        print program
        interpret program (repeat 0) 0 0 

main' :: IO()
main' = do
    file <- getLine
    withFile file ReadMode $ \handle -> do
        program <- hGetContents handle
        print program
        interpret program (repeat 0) 0 0 

interpret :: String -> [Int] -> Int -> Int -> IO ()
interpret prg memory idx rpos
    | rpos >= (length prg) = putChar '\n'
    | ch == '>' = interpret prg memory (idx+1) (rpos+1)
    | ch == '<' = if idx > 0 
                    then interpret prg memory (idx-1) (rpos+1)
                    else putStrLn "\nCan't allocate memory!"
    | ch == '+' = interpret prg (increment memory idx) idx (rpos+1)
    | ch == '-' = interpret prg (decrement memory idx) idx (rpos+1)
    | ch == '.' = do 
        putChar $ chr (memory !! idx)
        interpret prg memory idx (rpos+1)
    | otherwise = interpret prg memory idx (rpos+1)
        where ch = prg !! rpos

increment :: [Int] -> Int -> [Int]
increment lis x = f ++ [l+1] ++ ls
    where (f, (l:ls)) = splitAt x lis

decrement :: [Int] -> Int -> [Int]
decrement lis x = f ++ [l-1] ++ ls
    where (f, (l:ls)) = splitAt x lis

