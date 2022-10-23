import System.IO
import Parse
import Print
import Logic

{-Main function, responsible for the communication between the user and the program functions-}

main = do
    putStrLn "Write your polynomial:"
    a <- getLine
    let pol1 = reducePolynomial (parseInit a)
    putStrLn ""
    putStrLn "Your polynomial:"
    putStrLn $ polynomialToString pol1
    putStrLn ""
    putStrLn "What do you want to do:"
    putStrLn "1: Add another polynomial"
    putStrLn "2: Mutiply by other polynomial"
    putStrLn "3: Derive"
    ch <- getLine
    putStrLn ""
    choice ch pol1

choice :: String -> Polynomial -> IO()
choice ch pol1  | ch == "1" = do
                  putStrLn "Write another polynomial"
                  b <- getLine
                  let pol2 = reducePolynomial (parseInit b)
                  putStrLn ""
                  putStrLn "Your polynomial:"
                  putStrLn $ polynomialToString pol2
                  putStrLn ""
                  let res = addPolynomial pol1 pol2
                  putStrLn $ "Your result is: " ++ polynomialToString res
                | ch == "2" = do
                  putStrLn "Write another polynomial"
                  b <- getLine
                  let pol2 = reducePolynomial (parseInit b)
                  putStrLn ""
                  putStrLn "Your polynomial:"
                  putStrLn $ polynomialToString pol2
                  putStrLn ""
                  let res = multPolynomial pol1 pol2
                  putStrLn $ "Your result is: " ++ polynomialToString res
                | ch == "3" = do
                  putStrLn "In function of what variable?"
                  var <- getChar
                  b <- getLine --consume line
                  putChar var
                  putStrLn ""
                  let res = derivePolynomial pol1 var
                  putStrLn $ "Your result is: " ++ polynomialToString res
                | otherwise  = do
                  putStrLn "Invalid choice"
