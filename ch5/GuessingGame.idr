module Main


import Data.String
import System


guess : (target : Nat) -> (guesses : Nat) -> IO ()
guess target guesses = do
  putStr ("Guess " ++ show guesses ++ ": ")
  input <- getLine
  let newGuesses = guesses + 1
  case parsePositive input
  of
    Just num =>
      case compare (cast num) target of
        LT =>
          do
            putStrLn "Too low!"
            guess target newGuesses
        EQ =>
          do
            putStrLn "Just right!"
        GT =>
          do
            putStrLn "Too high!"
            guess target newGuesses
    Nothing =>
      do
        putStrLn "Must input a number"
        guess target newGuesses

main : IO ()
main = do
  t <- time
  guess (cast (mod t 100)) 1
