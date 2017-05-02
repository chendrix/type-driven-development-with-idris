module Main

import Data.Vect
import ReadVect


main : IO ()
main = do
  putStr "Enter your name: "
  x <- getLine
  putStrLn ("Hello " ++ x ++ "!")

printLonger : IO ()
printLonger = do
  putStr "First string: " >>= \_ =>
  getLine >>= \first =>
  putStr "Second string: " >>= \_ =>
  getLine >>= \second =>
    let
      len = max (length first) (length second)
    in
      putStrLn (show len)


readToBlank : IO (List String)
readToBlank = do
  x <- getLine
  if
    x == ""
  then
  do
    pure []
  else
  do
    xs <- readToBlank
    pure (x :: xs)


total join : String -> List String -> String
join sep [] = ""
join sep (x :: []) = x
join sep (x :: xs) = x ++ sep ++ join sep xs


readAndSave : IO ()
readAndSave = do
  putStrLn "Enter information (blank line to finish):"
  info <- readToBlank
  putStr "Save as (filename): "
  filename <- getLine
  Right _  <- writeFile filename (join "\n" info) |  Left err => putStrLn ("Error saving file: " ++ show err)
  putStrLn ("Saved as "  ++ show filename ++ "")


readVectFile : (filename : String) -> IO (n ** Vect n String)
readVectFile filename =
  do
    Right file <- openFile filename Read | Left err => do
      putStrLn ("Error opening file: " ++ show err)
      pure (_ ** [])
    helper file
  where
    helper :  File -> IO (m ** Vect m String)
    helper file = do
      isEnd <- fEOF file
      if
        isEnd
      then
      do
        closeFile file
        pure (_ ** [])
      else
      do
        Right x <- fGetLine file |
          Left err => do
            putStrLn ("Error reading file line: " ++ show err)
            pure (_ ** [])
        (_ ** xs) <- helper file
        pure (_ ** x :: xs)
