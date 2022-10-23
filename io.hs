import System.IO
import Parse
import Print
import Logic

main = do
    putStrLn "Write your polinomyal:"
    a <- getLine
    let pol1 = reducePolinomial (parseInit a)
    putStrLn ""
    putStrLn "Your polinomyal:"
    putStrLn $ polinomyalToString pol1
    putStrLn ""
    putStrLn "What do you want to do:"
    putStrLn "1: Add another polinomyal"
    putStrLn "2: Mutiply by other polinomyal"
    putStrLn "3: Derive"
    ch <- getLine
    putStrLn ""
    choice ch pol1

choice :: String -> Polinomyal -> IO()
choice ch pol1  | ch == "1" = do
                  putStrLn "Write another polinomyal"
                  b <- getLine
                  let pol2 = reducePolinomial (parseInit b)
                  putStrLn ""
                  putStrLn "Your polinomyal:"
                  putStrLn $ polinomyalToString pol2
                  putStrLn ""
                  let res = addPolinomial pol1 pol2
                  putStrLn $ "Your result is: " ++ polinomyalToString res
                | ch == "2" = do
                  putStrLn "Write another polinomyal"
                  b <- getLine
                  let pol2 = reducePolinomial (parseInit b)
                  putStrLn ""
                  putStrLn "Your polinomyal:"
                  putStrLn $ polinomyalToString pol2
                  putStrLn ""
                  let res = multPolinomial pol1 pol2
                  putStrLn $ "Your result is: " ++ polinomyalToString res
                | ch == "3" = do
                  putStrLn "In function of what variable?"
                  var <- getChar
                  b <- getLine --consume line
                  putChar var
                  putStrLn ""
                  let res = derivePolinomial pol1 var
                  putStrLn $ "Your result is: " ++ polinomyalToString res
                | otherwise  = do
                  putStrLn "Invalid choice"
