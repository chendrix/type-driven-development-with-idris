module Workspace

StringOrInt : Bool -> Type
StringOrInt False = String
StringOrInt True = Int


getStringOrInt : (isInt : Bool) -> StringOrInt isInt
getStringOrInt False = "Ninety four"
getStringOrInt True = 94


valToString : (isInt : Bool) -> (case isInt of False => String; True => Int) -> String
valToString False x = trim x
valToString True x = cast x


TupleVect : (len : Nat) -> (typ: Type) -> Type
TupleVect Z typ = ()
TupleVect (S k) typ = (typ, TupleVect k typ)
