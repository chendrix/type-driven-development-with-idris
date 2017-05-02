module Main

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
