module MyTests where

import Logic

prop_Add p1 p2 = poliEquals (addPolinomial p1 p2) (addPolinomial p2 p1)
  where types = (p1::Polinomyal, p2:: Polinomyal) 

prop_Mult p1 p2 = poliEquals (multPolinomial p1 p2) (multPolinomial p2 p1)
  where types = (p1::Polinomyal, p2:: Polinomyal) 

prop_Reduce p1 = poliEquals (reducePolinomial p1) (reducePolinomial (reducePolinomial p1))
  where types = p1::Polinomyal
