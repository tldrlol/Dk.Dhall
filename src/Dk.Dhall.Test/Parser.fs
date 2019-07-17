module Dk.Dhall.Parser.Test


open Xunit
open FParsec

open Dk.Dhall.Parser


let inline parse p s =
  run p s
  |> function
      | Success (s, _, _) -> Some s
      | Failure _         -> None


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

  let escape = function
    | "\"" -> "\\\""
    | "\\" -> "\\\\"
    | "\b" -> "\\b"
    | "\f" -> "\\f"
    | "\n" -> "\\n"
    | "\r" -> "\\r"
    | "\t" -> "\\t"
    | _    -> null

  let actual =
    escape input
    |> sprintf "\"%s\""
    |> parse doubleQuoteLiteral

  let expected =
    Some input

  Assert.Equal(expected, actual)


[<Theory>]
[<InlineData("\$")>]
[<InlineData("\/")>]
let ``doubleQuoteLiteral handles non .NET escape sequences``
  ( input: string ) =

  let actual =
    input
    |> sprintf "\"%s\""
    |> parse doubleQuoteLiteral

  let expected =
    Some (input.Substring 1)

  Assert.Equal(expected, actual)


[<Theory>]
[<InlineData("\\u2200")>]
[<InlineData("\\u{2200}")>]
[<InlineData("\\u{00002200}")>]
[<InlineData("\\u{00102200}")>]
let ``doubleQuoteLiteral handles unicode escape sequences``
  ( input: string ) =

  let actual =
    input
    |> sprintf "\"%s\""
    |> parse doubleQuoteLiteral

  let expected =
    System.Text.RegularExpressions.Regex.Match(input, @"\d+")
    |> fun x -> x.Value
    |> fun x -> System.Convert.ToInt32 (x, 16)
    |> System.Char.ConvertFromUtf32
    |> Some

  Assert.Equal(expected, actual)
