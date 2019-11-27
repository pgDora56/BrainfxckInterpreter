import System.IO
import System.Environment (getArgs)

main :: IO()
main = do
    args <- getArgs
    withFile (args !! 0) ReadMode $ \handle -> do
        program <- hGetContents handle
        putStrLn program
        interpret program

interpret :: String -> ()
interpret program =
