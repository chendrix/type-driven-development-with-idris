module Main

import Data.Vect

data DataStore : Type where
  MkData : (size : Nat) -> (items : Vect size String) -> DataStore


size : DataStore -> Nat
size (MkData size' items') = size'


items : (store : DataStore) -> Vect (size store) String
items (MkData size' items') = items'


addToStore : DataStore -> String -> DataStore
addToStore (MkData size items) newItem = MkData _ (addToData items)
  where
    addToData : Vect old String -> Vect (S old) String
    addToData [] = [newItem]
    addToData (x :: xs) = x :: addToData xs


data Command
  = Add String
  | Get Integer
  | Size
  | Search String
  | Quit


parseCommand : (cmd : String) -> (args : String) -> Maybe Command
parseCommand "add" str =
  Just (Add str)
parseCommand "get" val =
  case all isDigit (unpack val) of
    False => Nothing
    True => Just (Get (cast val))
parseCommand "quit" "" = Just Quit
parseCommand "size" "" = Just Size
parseCommand "search" str =
  Just (Search str)
parseCommand _ _ = Nothing


parse : (input : String) -> Maybe Command
parse input =
  case
    span (/= ' ') input
  of
    (cmd, args) =>
      parseCommand cmd (ltrim args)


getEntry : (pos : Integer) -> (store : DataStore) -> Maybe (String, DataStore)
getEntry pos store =
  let
    store_items = items store
  in
    case
      integerToFin pos (size store)
    of
      Nothing => Just ("Out of range\n", store)
      Just id =>
        Just (index id store_items ++ "\n", store)

searchEntry : (substr : String) -> (store : DataStore) -> Maybe (String, DataStore)
searchEntry substr store =
  let
    store_items = items store
    found = filter (isInfixOf substr) (toList store_items)
    response =
      foldr func "" found
  in
    Just (response ++ "\n", store)

  where
    func elem acc = elem ++ ", " ++ acc


processInput : DataStore -> String -> Maybe (String, DataStore)
processInput store input =
  case
    parse input
  of
    Nothing => Just ("Invalid command\n", store)
    Just (Add item) => Just ("ID " ++ show (size store) ++ "\n", addToStore store item)
    Just (Get pos) => getEntry pos store
    Just Size => Just (show (size store) ++ "\n", store)
    Just (Search substr) => searchEntry substr store
    Just Quit => Nothing


main : IO ()
main = replWith (MkData _ []) "Command: " processInput
