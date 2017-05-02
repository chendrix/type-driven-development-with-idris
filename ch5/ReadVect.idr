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


zipInputs : IO ()
zipInputs = do
  putStrLn "Enter your first vector (blank line to end):"
  (len1 ** vec1) <- readVect
  putStrLn "Enter your second vector (blank line to end):"
  (len2 ** vec2) <- readVect
  case
    exactLength len1 vec2
  of
    Nothing =>
      putStrLn "Could not zip the two (not the same length)"
    Just vec2' =>
      printLn (Data.Vect.zip vec1 vec2')
