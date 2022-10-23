
module Parse where
import Data.List
import Data.Char
import Logic

parseInit :: String -> Polinomyal
parseInit [] = []
parseInit str = map (parseNewTerm) terms
            where terms = splitTerms str

splitTerms :: String -> [String]
splitTerms [] = []
splitTerms (' ':str) = splitTerms str
splitTerms ('+':str) = splitTerms str
splitTerms ('-':str) = [('-':term)] ++ rest
                      where (term:rest) = splitTerms str
splitTerms str = (first:(splitTerms rest))
                where pairs = zip str (tail str)
                      f = (\(a,b) -> not ((a /= '^') && ((b == '-') || (b == '+') || (b == ' '))))
                      index = length (takeWhile f pairs) + 1
                      first = take index str
                      rest = drop index str

parseNewTerm :: String -> PolElement
parseNewTerm (' ':xs) = parseNewTerm xs
parseNewTerm (x:xs)   | isNum x = ((parseNum (takeWhile isNum (x:xs)) 0), (parseExp2 [] (dropWhile isNum (x:xs))))
                        | isLetter x = (1,parseVariable [(x, 1)] xs)
                        | otherwise  = error "Invalid Polinomyal"


parseMult :: [(Char,Float)] -> String -> [(Char,Float)]
parseMult vg (x:xs) | isLetter x = parseVariable (vg ++ [(x, 1)]) xs

--Can be terminal
parseVariable :: [(Char,Float)] -> String -> [(Char,Float)]
parseVariable vg [] = vg
parseVariable vg (x:xs) | x == '^' = parseExp1 vg xs
                        | x == '*' = parseMult vg xs
                        | otherwise = error "Invalid Polinomyal"  --inválido

parseExp1 :: [(Char,Float)] -> String -> [(Char,Float)]
parseExp1 vg (x:xs) | isNum x = parseExp2 (init vg ++ [(fst (last vg), (parseNum (takeWhile isNum (x:xs)) 0) )]) (dropWhile isNum (x:xs))
                    | otherwise = error "Invalid Polinomyal" --inválido

--Can be terminal
parseExp2 :: [(Char,Float)] -> String -> [(Char,Float)]
parseExp2 el [] = el
parseExp2 vg (x:xs) | x == '*' = parseMult vg xs
                    | isLetter x = parseVariable [(x, 1)] xs
                    | otherwise = error "Invalid Polinomyal" -- inválido

isNum :: Char -> Bool
isNum n = (n == '.') || (n == '-') || ((ord n >= 48) && (ord n <= 57))

parseDecimalNum :: String -> Int -> Float
parseDecimalNum [] n = 0
parseDecimalNum (x:xs) n = (fromIntegral (digitToInt x)) / (fromIntegral (10^n)) + (parseDecimalNum xs (n+1))

--Recieves string with only the numeral part wanted to parse
parseNum :: String -> Float -> Float
parseNum [] n = n
parseNum "-" 0 = -1
parseNum (x:xs) n   | ((ord x >= 48) && (ord x <= 57)) = parseNum xs (n*10 + (fromIntegral (digitToInt x)))
                    | x == '-' = -1 * (parseNum xs n)
                    | x == '.' = n + parseDecimalNum xs 1

moreThanOneDot :: String -> Bool
moreThanOneDot s = length (filter (=='.') s) > 1
