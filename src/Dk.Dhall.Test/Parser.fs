module Dk.Dhall.Parser.Test


open Xunit
open FParsec

open Dk.Dhall.Parser


[<Theory>]
[<InlineData("\"")>]
[<InlineData("\\")>]
[<InlineData("\b")>]
[<InlineData("\f")>]
[<InlineData("\n")>]
[<InlineData("\r")>]
[<InlineData("\t")>]
let ``doubleQuoteLiteral handles simple escape sequences``
  ( input: string ) =

  let unescape = function
    | "\"" -> "\\\""
    | "\\" -> "\\\\"
    | "\b" -> "\\b"
    | "\f" -> "\\f"
    | "\n" -> "\\n"
    | "\r" -> "\\r"
    | "\t" -> "\\t"
    | _    -> null

  let actual =
    input
    |> unescape
    |> sprintf "\"%s\""
    |> run doubleQuoteLiteral
    |> function
        | Success (s, _, _) -> Some s
        | Failure _         -> None

  let expected =
    Some input

  Assert.Equal(expected, actual)
