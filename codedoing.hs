import Data.List
import Data.Char

type PolElement = (Float, [(Char, Float)]) {-Coeficient, variable, grade-}
type Polinomyal = [PolElement]

{-data State
 = Init
 | NewTerm
 | Coef1
 | Coef2 -- num.
 | Mult -- num(.num)
 | Variable -- (num(.num))
 | Exp1
 | Exp2
 | End
 -}

parseInit :: String -> Polinomyal
parseInit [] = []
parseInit str = map parseNewTerm (splitTerms str)

splitTerms :: String -> [String]
splitTerms [] = []
splitTerms (' ':str) = splitTerms str
splitTerms ('+':str) = splitTerms str
splitTerms ('-':str) = [('-':term)] ++ rest
                      where (term:rest) = splitTerms str
splitTerms str = (first:(splitTerms rest))
                where first = takeWhile (\c -> c /=' ' && c /='+' && c /='-') str
                      rest = dropWhile (\c -> c /= ' ' && c /= '+' && c /= '-') str

parseNewTerm :: PolElement -> String -> PolElement
parseNewTerm el (' ':xs) = parseNewTerm el xs
parseNewTerm (c, vg) (x:xs) | isDigit x = parseCoef1 (c * fromIntegral (digitToInt x), vg) xs
                    | isLetter x = parseVariable (c, [(x, 0)])  xs
                    | otherwise  = (0, []) --inválido

--Can be terminal
parseCoef1 :: PolElement -> String -> PolElement
parseCoef1 el [] = el
parseCoef1 (c, vg) (x:xs) | isDigit x = parseCoef1 (c*10 + fromIntegral (digitToInt x), vg) xs
                       {-| x == '.' = parseCoef2  (c, vg) xs-}
                       | x == '*' = parseMult (c, vg) xs
                       | otherwise = (0, []) --inválido

--Can be terminal
{-
parseCoef2 :: PolElement -> String -> Float
parseCoef2 el [] = el
parseCoef2 (c, vg) (x:xs) | isDigit x = parseCoef2 (c*10 + (digitToInt x), vg) xs
                       | x == '.' = parseCoef2  (c, vg) xs
                       | x == '*' = parseMult (c, vg) xs
                       | otherwise = (0, []) --inválido
-}

parseMult :: PolElement -> String -> PolElement
parseMult (c, vg) (x:xs) | isLetter x = parseVariable (c, vg ++ [(x, 0)]) xs

--Can be terminal
parseVariable :: PolElement -> String -> PolElement
parseVariable el [] = el
parseVariable el (x:xs) | x == '^' = parseExp1 el xs
                       | otherwise = (0, [])  --inválido

parseExp1 :: PolElement -> String -> PolElement
parseExp1 (c, vg) (x:xs) | isDigit x = parseExp2 (c, init vg ++ [(fst (last vg), fromIntegral (digitToInt x) )]) xs
                         | otherwise = (0, []) --inválido

--Can be terminal
parseExp2 :: PolElement -> String -> PolElement
parseExp2 el [] = el
parseExp2 (c, vg) (x:xs) | isDigit x = parseExp2 (c, init vg ++ [(fst (last vg), (snd (last vg) * 10) + fromIntegral (digitToInt x))]) xs
                         | x == '*' = parseMult (c, vg) xs
                         | otherwise = (0, []) -- inválido
