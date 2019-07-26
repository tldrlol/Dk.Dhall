module Dk.Dhall.Parser.Test

open FParsec
open Xunit

open Dk.Dhall.Parser


let formatAssertEquals expected actual =
  sprintf
    "assertEqual Failure:\nExpected: %A\nActual:   %A"
    expected
    actual


type AssertEqualException<'a>(expected: 'a, actual: 'a) =
  inherit System.Exception(formatAssertEquals expected actual)


let assertEqual expected actual =
  if expected <> actual then 
    raise <| AssertEqualException (expected, actual)


let parse p s =
  run p s |> function
    | Success (s, _, _) -> Result.Ok s
    | Failure (s, e, u) -> Result.Error (s, e, u)


[<Theory>]
[<InlineData("\"")>]
[<InlineData("\\")>]
[<InlineData("\b")>]
[<InlineData("\f")>]
[<InlineData("\n")>]
[<InlineData("\r")>]
[<InlineData("\t")>]
let ``doubleQuoteLiteral handles simple escape sequences``
  (input: string) =

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
    Result.Ok input

  assertEqual expected actual


[<Theory>]
[<InlineData("\$")>]
[<InlineData("\/")>]
let ``doubleQuoteLiteral handles non .NET escape sequences``
  (input: string) =

  let actual =
    input
    |> sprintf "\"%s\""
    |> parse doubleQuoteLiteral

  let expected =
    Result.Ok (input.Substring 1)

  assertEqual expected actual


[<Theory>]
[<InlineData("\\u2200")>]
[<InlineData("\\u{2200}")>]
[<InlineData("\\u{00002200}")>]
[<InlineData("\\u{00102200}")>]
let ``doubleQuoteLiteral handles unicode escape sequences``
  (input: string) =

  let actual =
    input
    |> sprintf "\"%s\""
    |> parse doubleQuoteLiteral

  let expected =
    System.Text.RegularExpressions.Regex.Match(input, @"\d+")
    |> fun x -> x.Value
    |> fun x -> System.Convert.ToInt32 (x, 16)
    |> System.Char.ConvertFromUtf32
    |> Result.Ok

  assertEqual expected actual


let parseTest parser expected input =
  let actual   = parse parser input
  let expected = Result.Ok expected
  assertEqual expected actual


[<Theory>]
[<InlineData("''\n''", "")>]
[<InlineData("''''''''''", "''''")>]
[<InlineData("''\n''''''''", "''''")>]
[<InlineData("''''''''''''''''", "''''''''")>]
let ``singleQuoteLiteral works on simple examples``
  (input: string)
  (expected: string) =
  parseTest singleQuoteLiteral expected input


[<Theory>]
[<InlineData("-Infinity", System.Double.NegativeInfinity)>]
[<InlineData("Infinity",  System.Double.PositiveInfinity)>]
let ``doubleLiteral works on infinites``
  (input: string)
  (expected: double) =
  parseTest doubleLiteral expected input


// TODO: Clean this up a bit.
[<Fact>]
let ``doubleLiteral works on NaN`` () =
  match run doubleLiteral "NaN" with
  | Success (x, _, _) -> Assert.True (System.Double.IsNaN x)
  | Failure _         -> Assert.True false


open Hedgehog


[<Fact>]
let ``doubleLiteral parses string representations of System.Double`` () =
  Property.check <| property {
    let! expected = Gen.double (Range.linearBounded ())
    return
      match run doubleLiteral (expected.ToString("F17")) with
      | ParserResult.Success (actual, _, _) -> expected = actual
      | ParserResult.Failure _              -> false
  }
