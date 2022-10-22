import Data.List
import Data.Char

type PolElement = (Float, [(Char, Float)]) {-Coeficient, variable, grade-}
type Polinomyal = [PolElement]



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


{-Needs to receive an empty PolElement with 1 as the coef if positive and -1 if negative-}
parseNewTerm :: PolElement -> String -> PolElement
parseNewTerm el (' ':xs) = parseNewTerm el xs
parseNewTerm (c, vg) (x:xs) | isDigit x = parseCoef (c, vg) ([x] ++ xs)
                    | isLetter x = parseVariable (c, [(x, 0)])  xs
                    | otherwise  = (0, []) --inválido

parseCoef :: PolElement -> String -> PolElement
parseCoef (c, vg) xs = parseEndCoef (c * (parseNum (takeWhile (\c -> isDigit c || (c == '.')) xs) 0), vg) (dropWhile (\c -> isDigit c || (c == '.')) xs)

 --Can be terminal
parseEndCoef :: PolElement -> String -> PolElement
parseEndCoef el [] = el
parseEndCoef el (x:xs) | x == '*' = parseMult el xs
                       | otherwise = (0, [])  --inválido

parseMult :: PolElement -> String -> PolElement
parseMult (c, vg) (x:xs) | isLetter x = parseVariable (c, vg ++ [(x, 0)]) xs
                         | otherwise = (0, [])  --inválido

--Can be terminal
parseVariable :: PolElement -> String -> PolElement
parseVariable el [] = el
parseVariable el (x:xs) | x == '^' = parseExp el xs
                       | otherwise = (0, [])  --inválido

parseExp :: PolElement -> String -> PolElement
parseExp (c, vg) xs = parseEndExp (c, init vg ++ [(fst (last vg), (parseNum (takeWhile (\c -> isDigit c || (c == '.')) xs) 0))]) (dropWhile (\c -> isDigit c || (c == '.')) xs)


--Can be terminal
parseEndExp :: PolElement -> String -> PolElement
parseEndExp el [] = el
parseEndExp (c, vg) (x:xs) | x == '*' = parseMult (c, vg) xs
                           | otherwise = (0, []) -- inválido


parseDecimalNum :: String -> Int -> Float
parseDecimalNum [] n = 0
parseDecimalNum (x:xs) n = (fromIntegral (digitToInt x)) / (fromIntegral (10^n)) + (parseDecimalNum xs (n+1))

--Recieves string with only the numeral part wanted to parse
parseNum :: String -> Float -> Float
parseNum [] n = n
parseNum (x:xs) n   | isDigit x = parseNum xs (n*10 + (fromIntegral (digitToInt x)))
                   | x == '.' = n + parseDecimalNum xs 1
