module Printf


data Format
  = End
  | Number Format
  | Str Format
  | Lit String Format


PrintfType : Format -> Type
PrintfType End = String
PrintfType (Number x) = Int -> PrintfType x
PrintfType (Str x) = String -> PrintfType x
PrintfType (Lit x format) = PrintfType format


printfFmt : (fmt : Format) -> (acc : String) -> PrintfType fmt
printfFmt End acc = acc
printfFmt (Number fmt) acc = \i => printfFmt fmt (acc ++ (show i))
printfFmt (Str fmt) acc = \str => printfFmt fmt (acc ++ str)
printfFmt (Lit lit fmt) acc = printfFmt fmt (acc ++ lit)


total toFormat : (xs : List Char) -> Format
toFormat [] = End
toFormat ('%' :: 'd' :: chars) = Number (toFormat chars)
toFormat ('%' :: 's' :: chars) = Str (toFormat chars)
toFormat (c :: chars) =
  case
    toFormat chars
  of
    (Lit lit fmt) => Lit (strCons c lit) fmt
    fmt => Lit (strCons c "") fmt


printf : (fmt : String) -> PrintfType (toFormat (unpack fmt))
printf fmt = printfFmt _ ""
