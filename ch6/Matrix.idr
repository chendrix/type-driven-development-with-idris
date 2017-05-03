module Matrix


import Data.Vect


Matrix : (rows : Nat) -> (cols : Nat) -> Type
Matrix rows cols = Vect rows (Vect cols Double)
