import Data.Map
import Data.List

type PolElement = (Int, [(Char, Int)]) {-Coeficient, variable, grade-}
type Polinomyal = [PolElement]

reduceVG :: PolElement -> PolElement
reduceVG (c,vg) = (c, sortBy (\(a, _) (b, _) -> compare a b ) (toList (fromListWith (+) vg)))
