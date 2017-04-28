identity : a -> a
identity x = x

twice : (a -> a) -> (a -> a)
twice f = f . f

quadruple : Num a => a -> a
quadruple x = ?double (?double x)

palindrome : Nat -> String -> Bool
palindrome k x =
  if length x > k
    then
      toLower x == reverse (toLower x)
    else
      False

counts : String -> (Nat, Nat)
counts x =
  let
    numWords = length (words x)
    numChars = length x
  in
    (numWords, numChars)

top_ten : Ord a => List a -> List a
top_ten xs = take 10 (reverse (sort xs))

over_length : Nat -> List String -> Nat
over_length k xs = Prelude.List.length (filter (\x => Prelude.Strings.length x > k) xs)
