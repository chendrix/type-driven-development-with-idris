module Main

main : IO ()
main = putStrLn (?convert 'x')

StringOrInt : Bool -> Type
StringOrInt x = case x of
                     False => Int
                     True => String

valToString : (x : Bool) -> StringOrInt x -> String
valToString False y = cast y
valToString True y = y

average : (str : String) -> Double
average str = let numWords = wordCount str
                  totalLength = sum (allLengths (words str))
              in
                  cast totalLength / cast numWords
              where
                wordCount : String -> Nat
                wordCount x = length (words str)

                allLengths : List String -> List Nat
                allLengths xs = map length xs
