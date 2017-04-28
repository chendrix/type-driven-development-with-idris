module Workspace

import Data.Vect

invert : Bool -> Bool
invert False = True
invert True = False

total map : (a -> b) -> List a -> List b
map f [] = []
map f (x :: xs) = f x :: map f xs

total mapV : (a -> b) -> Vect n a -> Vect n b
mapV f [] = []
mapV f (x :: xs) = f x :: map f xs


allLengths : List String -> List Nat
allLengths [] = []
allLengths (x :: xs) = length x :: allLengths xs

total allL : Vect n String -> Vect n Nat
allL [] = []
allL (x :: xs) = 0 :: allL xs

xor : Bool -> Bool -> Bool
xor False y = y
xor True y = not y

mutual
  isEven : Nat -> Bool
  isEven Z = True
  isEven (S k) = isOdd k

  isOdd : Nat -> Bool
  isOdd Z = False
  isOdd (S k) = isEven k

insert : Ord elem => (x : elem) -> (Vect len elem) -> Vect (S len) elem
insert x [] = [x]
insert x (y :: xs) =
  if
    x <= y
  then
    x :: y :: xs
  else
    y :: insert x xs

total sort : Ord elem => Vect n elem -> Vect n elem
sort [] = []
sort (x :: xs) =
  let
    xsSorted = sort xs
  in
    insert x xsSorted

total length : List a -> Nat
length [] = Z
length (x :: xs) = S (Workspace.length xs)

total reverse : List a -> List a
reverse [] = []
reverse (x :: xs) = reverse xs ++ [x]
