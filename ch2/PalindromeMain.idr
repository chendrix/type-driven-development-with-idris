module Main

import Palindrome

p : String -> String
p x = (show (palindrome x)) ++ "\n"

main : IO ()
main = repl "Enter a string: " p
