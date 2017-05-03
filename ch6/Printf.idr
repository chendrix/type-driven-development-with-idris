module Printf


data Format
  = End
  | Number Format
  | Str Format
  | Ch Format
  | Doub Format
  | Lit String Format


total PrintfType : Format -> Type
PrintfType End = String
PrintfType (Number x) = Int -> PrintfType x
PrintfType (Str x) = String -> PrintfType x
PrintfType (Lit x format) = PrintfType format
PrintfType (Ch x) = Char -> PrintfType x
PrintfType (Doub x) = Double -> PrintfType x


total printfFmt : (fmt : Format) -> (acc : String) -> PrintfType fmt
printfFmt End acc = acc
printfFmt (Number fmt) acc = \i => printfFmt fmt (acc ++ (show i))
printfFmt (Str fmt) acc = \str => printfFmt fmt (acc ++ str)
printfFmt (Lit lit fmt) acc = printfFmt fmt (acc ++ lit)
printfFmt (Ch fmt) acc = \c => printfFmt fmt (acc ++ "'" ++ strCons c "'")
printfFmt (Doub fmt) acc = \d => printfFmt fmt (acc ++ cast d)


total toFormat : (xs : List Char) -> Format
toFormat [] = End
toFormat ('%' :: 'd' :: chars) = Number (toFormat chars)
toFormat ('%' :: 's' :: chars) = Str (toFormat chars)
toFormat ('%' :: 'c' :: chars) = Ch (toFormat chars)
toFormat ('%' :: 'f' :: chars) = Doub (toFormat chars)
toFormat (c :: chars) =
  case
    toFormat chars
  of
    (Lit lit fmt) => Lit (strCons c lit) fmt
    fmt => Lit (strCons c "") fmt


printf : (fmt : String) -> PrintfType (toFormat (unpack fmt))
printf fmt = printfFmt _ ""
