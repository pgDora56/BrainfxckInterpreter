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
        interpret program (repeat 0) 0 0 

interpret :: DB.ByteString -> [Int] -> Int -> Int -> IO ()
interpret prg memory idx rpos
    | rpos >= (DB.length prg) = return ()
    | (prg !! rpos) == "+" = do
        nmemo <- increment memory idx
        interpret prg nmemo idx (rpos+1)
    | (prg !! rpos) == "." = do 
        putStr $ chr (memory !! idx)
        interpret prg memory idx (rpos+1)

increment :: [Int] -> Int -> [Int]
increment lis x = f ++ [l+1] ++ ls
    where (f, (l:ls)) = splitAt x lis

