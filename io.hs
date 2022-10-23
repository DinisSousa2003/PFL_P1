import System.IO
import Parse
import Print

main = do
    putStrLn "Write your polinomyal:"
    a <- getLine
    putStrLn "What do you want to do:"
    putStrLn "1: Add another polinomyal"
    putStrLn "2: Mutiply by other polinomyal"
    putStrLn "3: Derive"
    ch <- getChar
    putChar ch
    choice ch

choice :: Char -> IO()
choice ch | ch == '1' = do
            putStrLn "Write another polinomyal"
            b <- getLine
            putStrLn "Your result is xxxxx"
          | ch == '2' = do
            putStrLn "Write another polinomyal"
            b <- getLine
            putStrLn "Your result is xxxxx"
          | ch == '3' = do
            putStrLn "Your result is xxxxx"
          | otherwise  = do
            putStrLn "Invalid choice"
