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
