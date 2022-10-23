module Print where
import Data.List
import Data.Char
import Logic

polinomyalToString :: Polinomyal -> String
polinomyalToString [] = "0"
polinomyalToString (x:[]) = polElementToString x
polinomyalToString (x:xs) = polElementToString x ++ " " ++ polinomyalToString xs

polElementToString :: PolElement -> String
polElementToString (c, vg) = signal ++ " " ++ show (abs c) ++ vgToString vg
                          where signal = if c > 0 then "+" else "-"

vgToString :: [(Char, Float)] -> String
vgToString [] = []
vgToString (x:xs) = "*" ++ [fst x] ++ "^" ++ show (snd x) ++ vgToString xs
