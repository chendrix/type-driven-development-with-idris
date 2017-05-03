module Adder


AdderType : (numargs : Nat) -> (t : Type) -> Type
AdderType Z t = t
AdderType (S k) t = (next : t) -> AdderType k t


adder : Num t => (numargs : Nat) -> (acc : t) -> AdderType numargs t
adder Z acc = acc
adder (S k) acc = (\next => adder k (acc + next))
