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


let inline isValidNonAscii (c: int) : bool =
  inRange 0x80     0xD7FF   c ||
  inRange 0xE000   0xFFFD   c ||
  inRange 0x10000  0x1FFFD  c ||
  inRange 0x20000  0x2FFFD  c ||
  inRange 0x30000  0x3FFFD  c ||
  inRange 0x40000  0x4FFFD  c ||
  inRange 0x50000  0x5FFFD  c ||
  inRange 0x60000  0x6FFFD  c ||
  inRange 0x70000  0x7FFFD  c ||
  inRange 0x80000  0x8FFFD  c ||
  inRange 0x90000  0x9FFFD  c ||
  inRange 0xA0000  0xAFFFD  c ||
  inRange 0xB0000  0xBFFFD  c ||
  inRange 0xC0000  0xCFFFD  c ||
  inRange 0xD0000  0xDFFFD  c ||
  inRange 0xE0000  0xEFFFD  c ||
  inRange 0xF0000  0xFFFFD  c ||
  inRange 0x100000 0x10FFFD c


let blockCommentChar
   =  codePointSatisfy (fun c -> inRange 0x20 0x7F c || isValidNonAscii c) >>% ()
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
    ( codePointSatisfy <| fun c ->
        inRange 0x20 0x7F c ||
        isValidNonAscii c   ||
        c = int '\t'
    )
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
    inRange 0x20 0x20 c ||
    inRange 0x23 0x5B c ||
    inRange 0x5D 0x7F c ||
    isValidNonAscii   c


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


let tab : Parser<string> =
  charReturn '\t' "\t"


let endOfLine : Parser<string>
   =  (charReturn '\n' "\n")
  <|> (skipChar '\r' >>. skipChar '\n' >>% "\r\n")


let singleQuoteChar : Parser<string>
   =  codePointSatisfy (fun c -> inRange 0x20 0x7F c || isValidNonAscii c)
  <|> tab
  <|> endOfLine


// TODO: Interpolation.
// TODO: Strip leading whitespace
let singleQuoteChunk : Parser<string> =
  let guard =
    notFollowedBy 
      (skipString "''" >>. notFollowedBy (skipChar '\'' <|> skipString "${"))
      
  guard >>. choice
    [ escapedQuotePair
      escapedInterpolation
      singleQuoteChar
    ]


let singleQuoteLiteral : Parser<string> =
  between
    (skipString "''" >>. opt skipNewline)
    (skipString "''")
    (manyStrings singleQuoteChunk)


let textLiteral : Parser<string>
   =  doubleQuoteLiteral
  <|> singleQuoteLiteral


let _keyword (s: string) : Parser<unit> =
  skipString s >>. skipMany1 whitespaceChunk


let _reserved (s: string) : Parser<unit> =
  skipString s >>. skipMany whitespaceChunk


let _if       : Parser<unit> = _keyword "if"
let _then     : Parser<unit> = _keyword "then"
let _else     : Parser<unit> = _keyword "else"
let _let      : Parser<unit> = _keyword "let"
let _in       : Parser<unit> = _keyword "in"
let _as       : Parser<unit> = _keyword "as"
let _using    : Parser<unit> = _keyword "using"
let _merge    : Parser<unit> = _keyword "merge"
let _Some     : Parser<unit> = _keyword "Some"
let _toMap    : Parser<unit> = _keyword "toMap"

let _missing  : Parser<unit> = _reserved "missing"
let _Infinity : Parser<unit> = _reserved "Infinity"
let _NaN      : Parser<unit> = _reserved "NaN"


let keyword : Parser<unit> =
  choice
    [ _if
      _then
      _else
      _let
      _in
      _using
      _missing
      _as
      _Infinity
      _NaN
      _merge
      _Some
      _toMap
    ]


let _Optional : Parser<unit> = _reserved "Optional"
let _Text     : Parser<unit> = _reserved "Text"
let _List     : Parser<unit> = _reserved "List"
let _Location : Parser<unit> = _reserved "Location"


let _Bool             : Parser<unit> = _reserved "Bool"
let _True             : Parser<unit> = _reserved "True"
let _False            : Parser<unit> = _reserved "False"
let _None             : Parser<unit> = _reserved "None"
let _Natural          : Parser<unit> = _reserved "Natural"
let _Integer          : Parser<unit> = _reserved "Integer"
let _Double           : Parser<unit> = _reserved "Double"
let _Type             : Parser<unit> = _reserved "Type"
let _Kind             : Parser<unit> = _reserved "Kind"
let _Sort             : Parser<unit> = _reserved "Sort"
let _NaturalFold      : Parser<unit> = _reserved "Natural/fold"
let _NaturalBuild     : Parser<unit> = _reserved "Natural/build"
let _NaturalIsZero    : Parser<unit> = _reserved "Natural/isZero"
let _NaturalEven      : Parser<unit> = _reserved "Natural/even"
let _NaturalOdd       : Parser<unit> = _reserved "Natural/odd"
let _NaturalToInteger : Parser<unit> = _reserved "Natural/toInteger"
let _NaturalShow      : Parser<unit> = _reserved "Natural/show"
let _IntegerToDouble  : Parser<unit> = _reserved "Integer/toDouble"
let _IntegerShow      : Parser<unit> = _reserved "Integer/show"
let _DoubleShow       : Parser<unit> = _reserved "Double/show"
let _ListBuild        : Parser<unit> = _reserved "List/build"
let _ListFold         : Parser<unit> = _reserved "List/fold"
let _ListLength       : Parser<unit> = _reserved "List/length"
let _ListHead         : Parser<unit> = _reserved "List/head"
let _ListLast         : Parser<unit> = _reserved "List/last"
let _ListIndexed      : Parser<unit> = _reserved "List/indexed"
let _ListReverse      : Parser<unit> = _reserved "List/reverse"
let _OptionalFold     : Parser<unit> = _reserved "Optional/fold"
let _OptionalBuild    : Parser<unit> = _reserved "Optional/build"
let _TextShow         : Parser<unit> = _reserved "Text/show"


let builtin : Parser<unit> =
  choice
    [ _NaturalFold
      _NaturalBuild
      _NaturalIsZero
      _NaturalEven
      _NaturalOdd
      _NaturalToInteger
      _NaturalShow
      _IntegerToDouble
      _IntegerShow
      _DoubleShow
      _ListBuild
      _ListFold
      _ListLength
      _ListHead
      _ListLast
      _ListIndexed
      _ListReverse
      _OptionalFold
      _OptionalBuild
      _TextShow
      _Bool
      _True
      _False
      _Optional
      _None
      _Natural
      _Integer
      _Double
      _Text
      _List
      _Type
      _Kind
      _Sort
    ]


let inline operator cp alt : Parser<unit> = 
  skipSatisfy ((=) cp) <|> skipString alt


let combine      = operator '\u2227'  """/\"""
let combineTypes = operator '\u2A53' """//\\"""
let prefer       = operator '\u2AFD' """//"""
let lambda       = operator '\u03BB' """\"""
let forall       = operator '\u2200' """forall"""
let arrow        = operator '\u2192' """->"""


let inline optOrEmpty p : Parser<string> =
  opt p |>> function | Some x -> string x | None -> ""


let exponent : Parser<string> =
  pipe3
    (pstring "e")
    (optOrEmpty (anyOf "+-"))
    (manyChars digit)
    (fun e sign digits -> e + sign + digits)


let minusInfinityLiteral : Parser<float> =
  skipChar '-' >>. _Infinity >>% System.Double.NegativeInfinity


let infinityLiteral : Parser<float> =
  _Infinity >>% System.Double.PositiveInfinity


let numericDoubleLiteral : Parser<double> =
  let suffix
     =  pipe3
          (pstring ".")
          (many1Chars digit)
          (optOrEmpty exponent)
          (fun dot digits exponent -> dot + digits + exponent)
    <|> exponent

  pipe3
    (optOrEmpty (anyOf "+-"))
    (many1Chars digit)
    (suffix)
    (fun sign digits suffix -> System.Double.Parse (sign + digits + suffix))


let doubleLiteral : Parser<double> =
  choice
    [ attempt numericDoubleLiteral
      minusInfinityLiteral
      infinityLiteral
      _NaN >>% System.Double.NaN
    ]
