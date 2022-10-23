module MyTests where

import Parse
import Print
import Logic

-- Test for the associative property of addition
prop_Add p1 p2 = polyEquals (addPolynomial p1 p2) (addPolynomial p2 p1)
  where types = (p1::Polynomial, p2:: Polynomial) 

-- Test for the associative property of multiplication
prop_Mult p1 p2 = polyEquals (multPolynomial p1 p2) (multPolynomial p2 p1)
  where types = (p1::Polynomial, p2:: Polynomial) 

-- Test for applying the reduction twice
prop_Reduce p1 = polyEquals (reducePolynomial p1) (reducePolynomial (reducePolynomial p1))
  where types = p1::Polynomial

-- Tests for all operations
main :: IO()
main = do
    putStrLn "\nTest 1: Reduce polynomial '-3.10*x^2*y^7.3 + 2*x^2*x^3'"
    putStrLn "Expected: - 3.1*x^2.0*y^7.3 + 2.0*x^5.0"
    putStr "Result  : "
    putStrLn (polynomialToString (reducePolynomial (parseInit "-3.10*x^2*y^7.3 + 2*x^2*x^3")))

    putStrLn "\nTest 2: Reduce polynomial '4 -x^-3 - x^-3.0 + 32'"
    putStrLn "Expected: - 2.0*x^-3.0 + 36.0"
    putStr "Result  : "
    putStrLn (polynomialToString (reducePolynomial (parseInit "4 -x^-3 - x^-3.0 + 32")))

    putStrLn "\nTest 3: Add polynomials '4 -x^-3 - x^-3.0 + 32' and '-3.10*x^2*y^7.3 + 2*x^2*x^3'"
    putStrLn "Expected: - 3.1*x^2.0*y^7.3 + 2.0*x^5.0 - 2.0*x^-3.0 + 36.0"
    putStr "Result  : "
    putStrLn (polynomialToString (addPolynomial (parseInit "4 -x^-3 - x^-3.0 + 32") (parseInit "-3.10*x^2*y^7.3 + 2*x^2*x^3")))

    putStrLn "\nTest 4: Add polynomials '0' and 'x'"
    putStrLn "Expected: + x"
    putStr "Result  : "
    putStrLn (polynomialToString (addPolynomial (parseInit "0") (parseInit "x")))

    putStrLn "\nTest 5: Multiply polynomials '4 -x^-3 - x^-3.0 + 32' and '-3.10*x^2*y^7.3 + 2*x^2*x^3'"
    putStrLn "Expected: - 111.6*x^2.0*y^7.3 + 6.2*x^-1.0*y^7.3 + 72.0*x^5.0 - 4.0*x^2.0"
    putStr "Result  : "
    putStrLn (polynomialToString (multPolynomial (parseInit "4 -x^-3 - x^-3.0 + 32") (parseInit "-3.10*x^2*y^7.3 + 2*x^2*x^3")))

    putStrLn "\nTest 6: Multiply polynomials '0' and 'x'"
    putStrLn "Expected: 0"
    putStr "Result  : "
    putStrLn (polynomialToString (multPolynomial (parseInit "0") (parseInit "x")))

    putStrLn "\nTest 7: Derive polynomial '4 -x^-3 - x^-3.0 + 32' for 'x'"
    putStrLn "Expected: + 6.0*x^-4.0"
    putStr "Result  : "
    putStrLn (polynomialToString (derivePolynomial (parseInit "4 -x^-3 - x^-3.0 + 32") 'x'))

    putStrLn "\nTest 8: Derive polynomial '2*x^2*y^3 - y^3 + 4*y^3 +0 - x' for 'y'"
    putStrLn "Expected: + 6.0*x^2.0*y^2.0 + 9.0*y^2.0"
    putStr "Result  : "
    putStrLn (polynomialToString (derivePolynomial (parseInit "2*x^2*y^3 - y^3 + 4*y^3 +0 - x") 'y'))
    
    putStrLn "\nTest 9: Derive polynomial '2*x^2*y^3 - y^3 + 4*y^3 +0 - x' for 'z'"
    putStrLn "Expected: 0"
    putStr "Result  : "
    putStrLn (polynomialToString (derivePolynomial (parseInit "2*x^2*y^3 - y^3 + 4*y^3 +0 - x") 'z'))

    