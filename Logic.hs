module Logic where
import Data.List

type PolElement = (Float, [(Char, Float)]) {-Coeficient, variable, grade-}
type Polinomyal = [PolElement]

replaceAtIndex :: Int -> a -> [a] -> [a]
replaceAtIndex idx elem xs = take idx xs ++ [elem] ++ drop (idx+1) xs

{-reduce the variable and grade list-}
fromListWith :: Fractional a => [(Char, a)] -> [(Char, a)]
fromListWith [] = []
fromListWith xs = [foldl (\(x, a) (_, b) -> (x,a + b)) (fstbase,0) xbases]  ++ fromListWith rest
                where fstbase = fst (head xs)
                      xbases = takeWhile (\(a, _) -> a == fstbase) xs
                      rest = dropWhile (\(a, _) -> a == fstbase) xs

reduceTerm :: PolElement -> PolElement
reduceTerm (c,vg) = (c, filter (\(_, a)-> a /= 0) reduced)
                where reduced = fromListWith (sortBy (\(a, _) (b, _) -> compare a b) vg)

{-reduce the variable and grade list-}
elemEqual :: PolElement -> PolElement -> Bool
elemEqual p1 p2 | (length xs) /= (length ys) = False
                | otherwise = all (==True) [(xs !! i) == (ys !! i) | i <- [0..(length xs - 1)]]
                where xs = snd (reduceTerm p1)
                      ys = snd (reduceTerm p2)

removeItem :: PolElement -> Polinomyal -> Polinomyal
removeItem _ [] = []
removeItem x (y:ys) | elemEqual x y = removeItem x ys
                    | otherwise     = y : removeItem x ys

sumEquals :: Polinomyal -> Polinomyal
sumEquals [] = []
sumEquals (x:xs) = [((fst x + sum [ k | (k,ks) <- xs, elemEqual x (k,ks)]),snd x)]  ++ sumEquals (removeItem x xs)

reducePolinomial :: Polinomyal -> Polinomyal
reducePolinomial p = sortBy (\a b -> compareTerms a b) (filter (\(c, _)-> c /= 0) (sumEquals reduced))
                    where reduced = map reduceTerm p

compareTerms :: PolElement -> PolElement -> Ordering
compareTerms (c1, xs) (c2, ys) | (length xs) /= (length ys) = compare (length ys) (length xs)
                               | compareVG xs ys /= EQ = compare xs ys
                               | otherwise = compareExp xs ys

compareVG :: [(Char,Float)] -> [(Char,Float)] -> Ordering
compareVG [] [] = EQ
compareVG (x:xs) (y:ys) | (fst x) /= (fst y) = compare (fst x) (fst y)
                        | otherwise = compareVG xs ys

compareExp :: [(Char,Float)] -> [(Char,Float)] -> Ordering
compareExp [] [] = EQ
compareExp (x:xs) (y:ys) | (snd x) /= (snd y) = compare (snd y) (snd x)
                         | otherwise = compareExp xs ys

poliEquals :: Polinomyal -> Polinomyal -> Bool
poliEquals p1 p2  | (length xs) /= (length ys) = False
                  | otherwise = all (==EQ) [compareTerms (xs !! i) (ys !! i) | i <- [0..(length xs - 1)]]
                  where xs = reducePolinomial p1
                        ys = reducePolinomial p2


addPolinomial :: Polinomyal -> Polinomyal -> Polinomyal
addPolinomial [] [] = []
addPolinomial p1 [] = p1
addPolinomial [] p2 = p2
addPolinomial p1 p2 = reducePolinomial (p1 ++ p2)

{-mutiply two polinomyals-}
auxMultPolinomial :: Polinomyal -> Polinomyal -> Polinomyal
auxMultPolinomial [] p2 = []
auxMultPolinomial p1 [] = []
auxMultPolinomial p1 (p2head : p2tail) = multPolinomialByElem p1 p2head ++ auxMultPolinomial p1 p2tail

multPolinomial :: Polinomyal -> Polinomyal -> Polinomyal
multPolinomial p1 p2 = reducePolinomial(auxMultPolinomial p1 p2)

{-mutiply all terms of a polinomyal by an element-}
multPolinomialByElem :: Polinomyal -> PolElement -> Polinomyal
multPolinomialByElem p1 pElem = Data.List.map (multPoliElem pElem) p1

{-mutiply two polynomials terms-}
multPoliElem :: PolElement -> PolElement -> PolElement
multPoliElem (c1, vg1) (c2, vg2) = reduceTerm ((c1 * c2), vg1 ++ vg2)

{-deriving polinomials, need to reduce in the end-}
derivePolinomial :: Polinomyal -> Char -> Polinomyal
derivePolinomial [] _ = []
derivePolinomial p var = reducePolinomial (auxDerivePolinomial p var)

auxDerivePolinomial :: Polinomyal -> Char -> Polinomyal
auxDerivePolinomial [] _ = []
auxDerivePolinomial (phead : ptail) var = [deriveElem phead idx] ++ auxDerivePolinomial ptail var
            where idx = findVarElem phead var

findVarElem :: PolElement -> Char -> Int
findVarElem (c, vg) var = case idx of
                              Just idx -> idx
                              Nothing -> -1
                        where idx = findIndex (\(ch, exp) -> ch == var) vg

deriveElem :: PolElement -> Int -> PolElement
deriveElem pol (-1) = pol
deriveElem (c, vg) idx = (c * exp, replaceAtIndex idx (ch, exp - 1) vg)
                    where (ch, exp) = vg !! idx
