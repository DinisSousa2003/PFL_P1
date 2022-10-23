module Logic where
import Data.List

type PolElement = (Float, [(Char, Float)]) {-(Coeficient, [(variable, grade)])-}
type Polynomial = [PolElement]

{-Replace the element on the index idx of the list xs by the element elem-}
replaceAtIndex :: Int -> a -> [a] -> [a]
replaceAtIndex idx elem xs = take idx xs ++ [elem] ++ drop (idx+1) xs

{-Reduce the variable and grade list-}
fromListWith :: Fractional a => [(Char, a)] -> [(Char, a)]
fromListWith [] = []
fromListWith xs = [foldl (\(x, a) (_, b) -> (x,a + b)) (fstbase,0) xbases]  ++ fromListWith rest
                where fstbase = fst (head xs)
                      xbases = takeWhile (\(a, _) -> a == fstbase) xs
                      rest = dropWhile (\(a, _) -> a == fstbase) xs

{-Reduce a term of a polynomial-}
reduceTerm :: PolElement -> PolElement
reduceTerm (c,vg) = (c, filter (\(_, a)-> a /= 0) reduced)
                where reduced = fromListWith (sortBy (\(a, _) (b, _) -> compare a b) vg)

{-Returns true if two PolElement are equal-}
elemEqual :: PolElement -> PolElement -> Bool
elemEqual p1 p2 | (length xs) /= (length ys) = False
                | otherwise = all (==True) [(xs !! i) == (ys !! i) | i <- [0..(length xs - 1)]]
                where xs = snd (reduceTerm p1)
                      ys = snd (reduceTerm p2)

{-Remove an PolElement from a Polynomial-}
removeItem :: PolElement -> Polynomial -> Polynomial
removeItem _ [] = []
removeItem x (y:ys) | elemEqual x y = removeItem x ys
                    | otherwise     = y : removeItem x ys

{-Sum two elements with the same variables and grades-}
sumEquals :: Polynomial -> Polynomial
sumEquals [] = []
sumEquals (x:xs) = [((fst x + sum [ k | (k,ks) <- xs, elemEqual x (k,ks)]),snd x)]  ++ sumEquals (removeItem x xs)

{-Reduce a polynomial to its normal form, by reducing each term and having the terms that have the same variables and grades summed, and then having with its terms ordered-}
reducePolynomial :: Polynomial -> Polynomial
reducePolynomial p = sortBy (\a b -> compareTerms a b) (filter (\(c, _)-> c /= 0) (sumEquals reduced))
                    where reduced = map reduceTerm p

{-Compare two PolElement, in order to order them, first by length, then by their variables and then by their exponents-}
compareTerms :: PolElement -> PolElement -> Ordering
compareTerms (c1, xs) (c2, ys) | (length xs) /= (length ys) = compare (length ys) (length xs)
                               | compareVG xs ys /= EQ = compare xs ys
                               | otherwise = compareExp xs ys

{-Compare variables of the elements-}
compareVG :: [(Char,Float)] -> [(Char,Float)] -> Ordering
compareVG [] [] = EQ
compareVG (x:xs) (y:ys) | (fst x) /= (fst y) = compare (fst x) (fst y)
                        | otherwise = compareVG xs ys

{-Compare exponents of variables of the elements-}
compareExp :: [(Char,Float)] -> [(Char,Float)] -> Ordering
compareExp [] [] = EQ
compareExp (x:xs) (y:ys) | (snd x) /= (snd y) = compare (snd y) (snd x)
                         | otherwise = compareExp xs ys

{-Returns true if two polynomials are equal to each other-}
polyEquals :: Polynomial -> Polynomial -> Bool
polyEquals p1 p2  | (length xs) /= (length ys) = False
                  | otherwise = all (==EQ) [compareTerms (xs !! i) (ys !! i) | i <- [0..(length xs - 1)]]
                  where xs = reducePolynomial p1
                        ys = reducePolynomial p2

{-Adding two polynomials, appending the second to the first and reducing-}
addPolynomial :: Polynomial -> Polynomial -> Polynomial
addPolynomial [] [] = []
addPolynomial p1 [] = p1
addPolynomial [] p2 = p2
addPolynomial p1 p2 = reducePolynomial (p1 ++ p2)

{-mutiply two polynomials-}
multPolynomial :: Polynomial -> Polynomial -> Polynomial
multPolynomial p1 p2 = reducePolynomial(auxMultPolynomial p1 p2)

{-Mutiply all terms of p1 by each term of p2, recursively-}
auxMultPolynomial :: Polynomial -> Polynomial -> Polynomial
auxMultPolynomial [] p2 = []
auxMultPolynomial p1 [] = []
auxMultPolynomial p1 (p2head : p2tail) = multPolynomialByElem p1 p2head ++ auxMultPolynomial p1 p2tail

{-Mutiply all terms of a polynomial by an element-}
multPolynomialByElem :: Polynomial -> PolElement -> Polynomial
multPolynomialByElem p1 pElem = Data.List.map (multPolyElem pElem) p1

{-Mutiply two polynomials terms-}
multPolyElem :: PolElement -> PolElement -> PolElement
multPolyElem (c1, vg1) (c2, vg2) = reduceTerm ((c1 * c2), vg1 ++ vg2)

{-Derive a polynomial in order to a given variable-}
derivePolynomial :: Polynomial -> Char -> Polynomial
derivePolynomial [] _ = []
derivePolynomial p var = reducePolynomial (auxDerivePolynomial p var)

{-Derive each term in order to the variable-}
auxDerivePolynomial :: Polynomial -> Char -> Polynomial
auxDerivePolynomial [] _ = []
auxDerivePolynomial (phead : ptail) var = [deriveElem phead idx] ++ auxDerivePolynomial ptail var
            where idx = findVarElem phead var

{-Find the position of the variable to be derived in the vg list of the element-}
findVarElem :: PolElement -> Char -> Int
findVarElem (c, vg) var = case idx of
                              Just idx -> idx
                              Nothing -> -1
                        where idx = findIndex (\(ch, exp) -> ch == var) vg

{-Derive the element itself (coef * exp and exp--)-}
deriveElem :: PolElement -> Int -> PolElement
deriveElem pol (-1) = (0,[])
deriveElem (c, vg) idx = (c * exp, replaceAtIndex idx (ch, exp - 1) vg)
                    where (ch, exp) = vg !! idx
