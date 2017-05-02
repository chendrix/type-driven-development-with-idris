module Repl

rep1 : (prompt : String) -> (onInput : String -> String) -> IO ()
rep1 prompt onInput = do
  putStr prompt
  line <- getLine
  putStr (onInput line)
  rep1 prompt onInput


rep1With : (state : a) -> (prompt : String) -> (onInput : a -> String -> Maybe (String, a)) -> IO ()
rep1With state prompt onInput = do
  putStr prompt
  input <- getLine
  case
    onInput state input
  of
    Nothing => pure ()
    Just (output, newState) => rep1With newState prompt onInput
