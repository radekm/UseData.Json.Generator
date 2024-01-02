module JsonGeneratorTest

open System

open NUnit.Framework

[<Test>]
let ``simple record`` () =
    let input = Ast.Record ("Person", [ { Name = "Name"; Type = Ast.JIdent ["string"] }
                                        { Name = "Age"; Type = Ast.JIdent ["int"] } ])
    let expected = [| "// TODO: Generate deserializer for record with fields Name, Age."
                      "static member ParseJson(v : UseData.Json.JsonValue) : Person = failwith \"TODO\""
                   |]
    let actual = JsonGenerator.run input
    Assert.That(actual, Is.EqualTo(expected))
