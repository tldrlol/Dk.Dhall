module Dk.Dhall.Parser

open FParsec
open Dk.Dhall.Util


type Parser<'a> = Parser<'a, unit>


let inline private inRange a b c =
  uint32 c - uint32 a <= uint32 b - uint32 a


let surrogatePair : Parser<int> =
  pipe2
    (satisfy (inRange '\uD800' '\uDBFF'))
    (satisfy (inRange '\uDC00' '\uDFFF'))
    (curry System.Char.ConvertToUtf32)


let codePointSatisfy predicate =

  let single =
    satisfy (int >> predicate) |>> string

  let pair = surrogatePair >>= fun c ->
    if predicate c
    then preturn (System.Char.ConvertFromUtf32 c)
    else fail (sprintf "Code point %x did not meet predicate" c)

  pair <|> single


let blockCommentChar
   =  codePointSatisfy (inRange 0x20 0X10FFFF) >>% ()
  <|> skipNewline
  <|> skipChar '\t'


let blockComment,         blockCommentRef         = createParserForwardedToRef()
let blockCommentContinue, blockCommentContinueRef = createParserForwardedToRef()

do
  blockCommentRef := 
    pstring "{-" >>. blockCommentContinue

  blockCommentContinueRef
    :=  skipString "-}"
    <|> (blockComment     >>. blockCommentContinue)
    <|> (blockCommentChar >>. blockCommentContinue)


let lineComment : Parser<unit> =
  pstring "--" >>.
  skipManyTill
    (codePointSatisfy (fun c -> inRange 0x20 0x10FFFF c || c = int '\t'))
    skipNewline


let whitespaceChunk : Parser<unit> =
  choice [ spaces1; lineComment; blockComment ]


// TODO: Simple labels can't be keywords.
let simpleLabel : Parser<string> =
  many1Satisfy2
    (fun c -> isAsciiLetter c || c = '_')
    (fun c -> isAsciiLetter c || isDigit c || c = '-' || c = '/' || c = '_')


let quotedLabel : Parser<string> =
  many1Satisfy <| fun c ->
    inRange '\u0020' '\u005F' c ||
    inRange '\u0061' '\u007E' c


let label : Parser<string>
   =  between (skipChar '`') (skipChar '`') quotedLabel
  <|> simpleLabel


// TODO: Non-reserved labels can't be reserved identifiers.
let nonReservedLabel : Parser<string> =
  label


let anyLabel : Parser<string> =
  label


let unicodeEscape : Parser<string> =

  let hexNumber = hex |>> function
    | c when isDigit c ->       int c              - int '0'
    | c                -> 10 + (int c ||| int ' ') - int 'a'

  let toNumber =
    Seq.reduce (fun a b -> a * 16 + b)

  let rec toNumberP n = function
    | [] ->
        preturn (System.Char.ConvertFromUtf32 n)
    | x :: xs ->
        let n = n * 16 + x
        if n > 0x10FFFF
        then fail "Unicode literal out of bounds"
        else toNumberP n xs

  in
    between
      (skipChar '{')
      (skipChar '}')
      (many1 hexNumber >>= toNumberP 0)
 <|>
    (parray 4 hexNumber |>> (toNumber >> char >> string))


let doubleQuoteEscaped : Parser<string> =
  choice
    [ pstring "\""
      pstring "$"
      pstring "\\"
      pstring "/"
      charReturn 'b' "\b"
      charReturn 'f' "\f"
      charReturn 'n' "\n"
      charReturn 'r' "\r"
      charReturn 't' "\t"
      skipChar 'u' >>. unicodeEscape
    ]

let doubleQuoteChar: Parser<string> =
  codePointSatisfy <| fun c ->
    inRange 0x20 0x20     c ||
    inRange 0x23 0x5B     c ||
    inRange 0x5D 0x10FFFF c


// TODO: Interpolation.
let doubleQuoteChunk : Parser<string>
   =  doubleQuoteChar
  <|> (skipChar '\\' >>. doubleQuoteEscaped)


let doubleQuoteLiteral : Parser<string> =
  between
    (skipChar '"')
    (skipChar '"')
    (manyStrings doubleQuoteChunk)


let escapedQuotePair : Parser<string> =
  skipString "'''" >>% "''"


let escapedInterpolation : Parser<string> =
  skipString "''${" >>% "${"


let endOfLine : Parser<string>
   =  (skipChar '\n' >>% "\n")
  <|> (skipChar '\r' >>. skipChar '\n' >>% "\r\n")


let singleQuoteChar : Parser<string>
   =  codePointSatisfy (inRange 0x20 0X10FFFF)
  <|> (skipChar '\t' >>% "\t")
  <|> endOfLine


// TODO: Interpolation.
let singleQuoteChunk : Parser<string>
  //  =  escapedQuotePair
  // <|> escapedInterpolation
  // <|> singleQuoteChar
   =  singleQuoteChar


let singleQuoteLiteral : Parser<string> =
  between
    (skipString "''" >>. skipNewline)
    (skipString "''")
    (manyStrings singleQuoteChar)


let textLiteral : Parser<string>
   =  doubleQuoteLiteral
  <|> singleQuoteLiteral
