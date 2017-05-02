module Main

import System

countdown : (secs : Nat) -> IO ()
countdown Z = putStrLn "Lift off!"
countdown left@(S secs) =
  do
    printLn left
    usleep 1000000
    countdown secs
