{-# LANGUAGE OverloadedStrings #-}

import System.IO
import System.Environment (getArgs)
import Data.Char (chr)
import qualified Data.ByteString.Char8 as DB

main :: IO()
main = do
    args <- getArgs
    withFile (args !! 0) ReadMode $ \handle -> do
        program <- DB.hGetContents handle
        print program
        interpret program (repeat 0) 0

interpret :: DB.ByteString -> [Int] -> Int -> IO ()
interpret "" _ _ = return ()
interpret (p:prg) memory idx 
    | p == "+" = do
        nmemo <- increment  memory idx
        interpret prg nmemo idx
    | p == "." = do 
        putStr $ chr (memory !! idx)
        interpret prg memory idx

increment :: [Int] -> Int -> [Int]
increment lis x = f ++ [l+1] ++ ls
    where (f, (l:ls)) = splitAt x lis

