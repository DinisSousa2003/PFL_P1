module Print where
import Data.List
import Data.Char
import Logic

{-Module responsible for printing strings-}

--Append the elements recursively into a string
polynomialToString :: Polynomial -> String
polynomialToString [] = "0"
polynomialToString (x:[]) = polElementToString x
polynomialToString (x:xs) = polElementToString x ++ " " ++ polynomialToString xs

--Transform coeficient into string and append to the variables and grades
polElementToString :: PolElement -> String
polElementToString (c, vg) = signal ++ " " ++ show (abs c) ++ vgToString vg
                          where signal = if c > 0 then "+" else "-"

--Transform the variables and grades to string recursively
vgToString :: [(Char, Float)] -> String
vgToString [] = []
vgToString (x:xs) = "*" ++ [fst x] ++ "^" ++ show (snd x) ++ vgToString xs
