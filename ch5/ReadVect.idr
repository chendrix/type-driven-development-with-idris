module ReadVect


import Data.Vect


readVectLen : (len : Nat) -> IO (Vect len String)
readVectLen Z = pure []
readVectLen (S k) = do
  x <- getLine
  xs <- readVectLen k
  pure (x :: xs)


data VectUnknown : Type -> Type where
  MkVect : (len : Nat) -> Vect len a -> VectUnknown a


readVect : IO (len ** Vect len String)
readVect = do
  x <- getLine
  if
    x == ""
  then
    pure (_ ** [])
  else
    do
      (_ ** xs) <- readVect
      pure ( _ ** (x :: xs))



printVect : Show a => (len ** Vect len a) -> IO ()
printVect (len ** xs) = do
  putStrLn (show xs ++ " (length " ++ show len ++ ")")
