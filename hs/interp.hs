{-# LANGUAGE OverloadedStrings #-}

import System.IO
import System.Environment (getArgs)
import Data.Char (chr, ord)

main :: IO()
main = do
    args <- getArgs
    withFile (args !! 0) ReadMode $ \handle -> do
        program <- hGetContents handle
        interpret program (repeat 0) [] 0 0 

-- For Debug in GHCi
main' :: IO()
main' = do
    file <- getLine
    withFile file ReadMode $ \handle -> do
        program <- hGetContents handle
        print program
        interpret program (repeat 0) [] 0 0 

interpret :: String -> [Int] -> [Int] -> Int -> Int -> IO ()
interpret prg memory while idx rpos
    | rpos >= (length prg) = putChar '\n'
    | ch == '>' = interpret prg memory while (idx+1) (rpos+1)
    | ch == '<' = if idx > 0 
                    then interpret prg memory while (idx-1) (rpos+1)
                    else putStrLn "\nCan't allocate memory!"
    | ch == '+' = interpret prg (increment memory idx) while idx (rpos+1)
    | ch == '-' = interpret prg (decrement memory idx) while idx (rpos+1)
    | ch == '[' = interpret prg memory ((rpos+1):while) idx (rpos+1)
    | ch == ']' = if (length while) > 0 then
                    if (memory !! idx) == 0
                        then interpret prg memory (tail while) idx (rpos+1)
                        else interpret prg memory while idx (head while)
                  else putStrLn "\n[ isn't found!"
    | ch == '.' = do 
        putChar $ chr (memory !! idx)
        interpret prg memory while idx (rpos+1)
    | ch == ',' = do
        putStr "\nInput > "
        ich <- getChar
        interpret prg (insert memory idx $ ord ich) while idx (rpos+1)
    | otherwise = interpret prg memory while idx (rpos+1)
        where ch = prg !! rpos

increment :: [Int] -> Int -> [Int]
increment lis x = f ++ [l+1] ++ ls
    where (f, (l:ls)) = splitAt x lis

decrement :: [Int] -> Int -> [Int]
decrement lis x = f ++ [l-1] ++ ls
    where (f, (l:ls)) = splitAt x lis

insert :: [Int] -> Int -> Int -> [Int]
insert lis x val = f ++ (val : ls)
    where (f, (_:ls)) = splitAt x lis

