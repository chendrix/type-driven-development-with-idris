module Palindrome

export
palindrome : String -> Bool
palindrome x = toLower x == reverse (toLower x)
