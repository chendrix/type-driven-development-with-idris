module Workspace

data Expr num
  = Val num
  | Add (Expr num) (Expr num)
  | Sub (Expr num) (Expr num)
  | Mul (Expr num) (Expr num)
  | Div (Expr num) (Expr num)
  | Abs (Expr num)

Num ty => Num (Expr ty) where
  (+) = Add
  (*) = Mul
  fromInteger = Val . fromInteger

Neg ty => Neg (Expr ty) where
  negate x = 0 - x
  (-) = Sub
  abs = Abs

Integral ty => Integral (Expr ty) where
  div = Div

Show ty => Show (Expr ty) where
  show (Val x) = show x
  show (Add x y) = "(" ++ show x ++ " + "++ show y ++ ")"
  show (Sub x y) = "(" ++ show x ++ " - "++ show y ++ ")"
  show (Mul x y) = "(" ++ show x ++ " * "++ show y ++ ")"
  show (Div x y) = "(" ++ show x ++ " / "++ show y ++ ")"
  show (Abs x) = "|" ++ show x ++ "|"


eval : (Integral ty, Neg ty) => Expr ty -> ty
eval (Val x) = x
eval (Add x y) = eval x + eval y
eval (Sub x y) = eval x - eval y
eval (Mul x y) = eval x * eval y
eval (Div x y) = eval x `div` eval y
eval (Abs x) = abs (eval x)


(Eq ty, Integral ty, Neg ty) => Eq (Expr ty) where
  (==) x y = eval x == eval y


(Integral ty, Neg ty) => Cast (Expr ty) ty where
  cast = eval
