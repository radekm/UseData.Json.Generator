module JsonGeneratorTest

open System

open NUnit.Framework

[<Test>]
let ``simple record`` () =
    let input = Ast.Record ("Person", [ { Name = "Name"; Type = Ast.JIdent ["string"] }
                                        { Name = "Age"; Type = Ast.JIdent ["int"] } ])
    let expected = [| "static member ParseJson(v : UseData.Json.JsonValue) : Person ="
                      "    { Name = v |> UJson.field \"Name\" UJson.string"
                      "      Age = v |> UJson.field \"Age\" UJson.int"
                      "    }"
                   |]
    let actual = JsonGenerator.run input
    Assert.That(actual, Is.EqualTo(expected))

[<Test>]
let ``record with optional fields`` () =
    let input = Ast.Record ("Foo", [ { Name = "Bar"; Type = Ast.JApp (Ast.JIdent ["voption"], [Ast.JIdent ["bool"]]) }
                                     { Name = "Baz"; Type = Ast.JApp (Ast.JIdent ["option"], [Ast.JIdent ["int"]]) } ])
    let expected =
        [| "static member ParseJson(v : UseData.Json.JsonValue) : Foo ="
           "    { Bar = v |> UJson.fieldOpt \"Bar\" UJson.bool"
           "      Baz = v |> UJson.fieldOpt \"Baz\" UJson.int |> function ValueNone -> None | ValueSome x -> Some x"
           "    }"
        |]
    let actual = JsonGenerator.run input
    Assert.That(actual, Is.EqualTo(expected))


[<Test>]
let ``record with list and array fields`` () =
    let input = Ast.Record ("Foo", [ { Name = "Bar"; Type = Ast.JApp (Ast.JIdent ["list"], [Ast.JIdent ["int64"]]) }
                                     { Name = "Baz"; Type = Ast.JArray (Ast.JIdent ["DateTimeOffset"]) } ])
    let parseDateTimeOffset = "fun s -> DateTimeOffset.Parse(s, System.Globalization.CultureInfo.InvariantCulture)"
    let expected =
        [| "static member ParseJson(v : UseData.Json.JsonValue) : Foo ="
           "    { Bar = v |> UJson.field \"Bar\" (UJson.map UJson.int64) |> Array.toList"
           $"      Baz = v |> UJson.field \"Baz\" (UJson.map (UJson.string >> %s{parseDateTimeOffset}))"
           "    }"
        |]
    let actual = JsonGenerator.run input
    Assert.That(actual, Is.EqualTo(expected))
