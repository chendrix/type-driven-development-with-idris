module Main

import Data.Vect

infixr 5 .+.

data Schema
  = SString
  | SInt
  | SChar
  | (.+.) Schema Schema


SchemaType : Schema -> Type
SchemaType SString = String
SchemaType SInt = Int
SchemaType SChar = Char
SchemaType (x .+. y) = (SchemaType x, SchemaType y)


record DataStore where
  constructor MkData
  schema : Schema
  size : Nat
  items : Vect size (SchemaType schema)


addToStore : (store : DataStore) -> (SchemaType (schema store)) -> DataStore
addToStore (MkData schema size items) newItem = MkData _ _ (addToData items)
  where
    addToData : Vect old (SchemaType schema) -> Vect (S old) (SchemaType schema)
    addToData [] = [newItem]
    addToData (x :: xs) = x :: addToData xs


data Command : Schema -> Type where
  SetSchema : (newschema : Schema) -> Command schema
  Add : SchemaType schema -> Command schema
  Get : Integer -> Command schema
  GetAll : Command schema
  Size : Command schema
  Quit : Command schema


parsePrefix : (schema : Schema) -> String -> Maybe (SchemaType schema, String)
parsePrefix SString input = getQuoted (unpack input)
  where
    getQuoted : List Char -> Maybe (String, String)
    getQuoted ('"' :: xs) =
      case
        span (/= '"') xs
      of
        (quoted, '"' :: rest) => Just (pack quoted, ltrim (pack rest))
        _ => Nothing
    getQuoted _ = Nothing
parsePrefix SInt input =
  case
    span isDigit input
  of
    ("", rest) => Nothing
    (num, rest) => Just (cast num, ltrim rest)
parsePrefix SChar input = getQuoted (unpack input)
  where
    getQuoted : List Char -> Maybe (Char, String)
    getQuoted ('\'' :: xs) =
      case
        span (/= '\'') xs
      of
        ([quoted], '\'' :: rest) => Just (quoted, ltrim (pack rest))
        _ => Nothing
    getQuoted _ = Nothing
parsePrefix (schema1 .+. schema2) input = do
  (l_schema, input') <- parsePrefix schema1 input
  (r_schema, r_rest) <- parsePrefix schema2 input'
  pure ((l_schema, r_schema), r_rest)


parseBySchema : (schema : Schema) -> String -> Maybe (SchemaType schema)
parseBySchema schema input =
  case
    parsePrefix schema input
  of
    Just (res, "") => Just res
    Just _ => Nothing
    Nothing => Nothing


parseSchema : List String -> Maybe Schema
parseSchema ["String"] = Just SString
parseSchema ("String" :: rest) = do
  schema <- parseSchema rest
  pure (SString .+. schema)
parseSchema ["Int"] = Just SInt
parseSchema ("Int" :: rest) = do
  schema <- parseSchema rest
  pure (SInt .+. schema)
parseSchema ["Char"] = Just SChar
parseSchema ("Char" :: rest) = do
  schema <- parseSchema rest
  pure (SChar .+. schema)
parseSchema _ = Nothing


parseCommand : (schema : Schema) -> (cmd : String) -> (args : String) -> Maybe (Command schema)
parseCommand schema "schema" rest =
  map SetSchema (parseSchema (words rest))
parseCommand schema "add" str =
  map Add (parseBySchema schema str)
parseCommand _ "get" "" = Just GetAll
parseCommand _ "get" val =
  case all isDigit (unpack val) of
    False => Nothing
    True => Just (Get (cast val))
parseCommand _ "quit" "" = Just Quit
parseCommand _ "size" "" = Just Size
parseCommand _ _ _ = Nothing


parse : (schema : Schema) -> (input : String) -> Maybe (Command schema)
parse schema input =
  case
    span (/= ' ') input
  of
    (cmd, args) =>
      parseCommand schema cmd (ltrim args)


total display : SchemaType schema -> String
display {schema = SString} item = show item
display {schema = SInt} item = show item
display {schema = SChar} item = show item
display {schema = (x .+. y)} (a, b) = display a ++ ", " ++ display b


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
        Just (display (index id store_items) ++ "\n", store)


showAllEntries : Nat -> (Vect n (SchemaType schema)) -> String
showAllEntries k [] = ""
showAllEntries k (x :: xs) =  show k ++ ": " ++ display x ++ "\n" ++ showAllEntries (k + 1) xs


setSchema : (schema : Schema) -> DataStore -> Maybe DataStore
setSchema schema store =
  case
    size store
  of
    Z => Just (MkData schema _ [])
    (S k) => Nothing


processInput : DataStore -> String -> Maybe (String, DataStore)
processInput store input =
  case
    parse (schema store) input
  of
    Nothing => Just ("Invalid command\n", store)
    Just (Add item) => Just ("ID " ++ show (size store) ++ "\n", addToStore store item)
    Just (Get pos) => getEntry pos store
    Just Size => Just (show (size store) ++ "\n", store)
    Just GetAll => Just (showAllEntries 0 (items store), store)
    Just (SetSchema schema') =>
      case
        setSchema schema' store
      of
        Nothing => Just ("Can't update schema when entries in store\n", store)
        Just store' => Just ("OK\n", store')
    Just Quit => Nothing


main : IO ()
main = replWith (MkData SString _ []) "Command: " processInput
