module Matrix

import Data.Vect


map2 : (a -> b -> c) -> Vect n a -> Vect n b -> Vect n c
map2 f [] [] = []
map2 f (x :: xs) (y :: ys) = f x y :: map2 f xs ys


add : Num numType => Vect rows (Vect cols numType) -> Vect rows (Vect cols numType) -> Vect rows (Vect cols numType)
add [] [] = []
add (x :: xs) (y :: ys) = map2 (+) x y :: add xs ys



total multiplyHelper : Num numType => (row : Vect m numType) -> (transposedY : Vect p (Vect m numType)) -> Vect p numType
multiplyHelper row [] = []
multiplyHelper row (x :: xs) = sum (Data.Vect.zipWith (*) row x) :: multiplyHelper row xs


total multiply : Num numType => Vect n (Vect m numType) -> Vect m (Vect p numType) -> Vect n (Vect p numType)
multiply [] [] = []
multiply [] (y :: ys) = replicate _ (replicate _ 0)
multiply (row :: rows) y =
  let
    transposedY = transpose y
  in
    multiplyHelper row transposedY :: multiply rows y




createEmpties : Vect m (Vect 0 elem)
createEmpties = replicate _ []

transpose : Vect n (Vect m elem) -> Vect m (Vect n elem)
transpose [] = createEmpties
transpose (x :: xs) =
  let
    xsTrans = Matrix.transpose xs
  in
    zipWith (::) x xsTrans
