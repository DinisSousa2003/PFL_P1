
module Parse where
import Data.List
import Data.Char
import Logic

{-Module responsible for the parsing of a string into a polynomial-}

--Map all the smaller strings into PolElement
parseInit :: String -> Polynomial
parseInit [] = []
parseInit str = map (parseNewTerm) terms
            where terms = splitTerms str

--Split the string in smaller strings, each representing a term of the polynomial
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

--Parse the string of an element into the element itself
parseNewTerm :: String -> PolElement
parseNewTerm (' ':xs) = parseNewTerm xs
parseNewTerm (x:xs)   | isNum x = ((parseNum (takeWhile isNum (x:xs)) 0), (parseExp2 [] (dropWhile isNum (x:xs))))
                        | isLetter x = (1,parseVariable [(x, 1)] xs)
                        | otherwise  = error "Invalid Polynomial"

--Called after receiving '*', must be followed by a letter
parseMult :: [(Char,Float)] -> String -> [(Char,Float)]
parseMult vg (x:xs) | isLetter x = parseVariable (vg ++ [(x, 1)]) xs
                    | otherwise  = error "Invalid Polynomial"

--Called after receiving a variable, can be followed by nothing (last variable), a '^' or a '*' (if the exponent is 1)
parseVariable :: [(Char,Float)] -> String -> [(Char,Float)]
parseVariable vg [] = vg
parseVariable vg (x:xs) | x == '^' = parseExp1 vg xs
                        | x == '*' = parseMult vg xs
                        | otherwise = error "Invalid Polynomial"  --inválido

--Called after a '^', to read a variable exponent
parseExp1 :: [(Char,Float)] -> String -> [(Char,Float)]
parseExp1 vg (x:xs) | isNum x = parseExp2 (init vg ++ [(fst (last vg), (parseNum (takeWhile isNum (x:xs)) 0) )]) (dropWhile isNum (x:xs))
                    | otherwise = error "Invalid Polynomial" --inválido

--Called after the exponent is read (or after the coeficient), can be followed by nothing (last variable) or a '*'
parseExp2 :: [(Char,Float)] -> String -> [(Char,Float)]
parseExp2 el [] = el
parseExp2 vg (x:xs) | x == '*' = parseMult vg xs
                    | otherwise = error "Invalid Polynomial" -- inválido

--Returns true if the value read is "numeric" ('.', '-', or any number)
isNum :: Char -> Bool
isNum n = (n == '.') || (n == '-') || ((ord n >= 48) && (ord n <= 57))

--Parse the part of the number that comes after the decimal separator
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
