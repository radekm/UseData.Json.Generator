module JsonGeneratorTest

open System

open NUnit.Framework

[<Test>]
let ``simple record`` () =
    let input = Ast.Record ("Person", [], [ { Name = "Name"; Type = Ast.JIdent ["string"] }
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
    let input = Ast.Record ("Foo",
                            [],
                            [ { Name = "Bar"; Type = Ast.JApp (Ast.JIdent ["voption"], [Ast.JIdent ["bool"]]) }
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
    let input = Ast.Record ("Foo",
                            [],
                            [ { Name = "Bar"; Type = Ast.JApp (Ast.JIdent ["list"], [Ast.JIdent ["int64"]]) }
                              { Name = "Baz"; Type = Ast.JArray (Ast.JIdent ["DateTimeOffset"]) } ])
    let parseDateTimeOffset = "fun s -> DateTimeOffset.Parse(s, System.Globalization.CultureInfo.InvariantCulture)"
    let expected =
        [| "static member ParseJson(v : UseData.Json.JsonValue) : Foo ="
           "    { Bar = v |> UJson.field \"Bar\" ((UJson.map UJson.int64) |> Array.toList)"
           $"      Baz = v |> UJson.field \"Baz\" (UJson.map (UJson.string >> %s{parseDateTimeOffset}))"
           "    }"
        |]
    let actual = JsonGenerator.run input
    Assert.That(actual, Is.EqualTo(expected))

[<Test>]
let ``record with optional array of optional values`` () =
    let input = Ast.Record ("Foo", [], [ { Name = "Baz"
                                           Type =
                                               Ast.JApp (Ast.JIdent ["voption"],
                                                         [Ast.JArray (Ast.JApp (Ast.JIdent ["option"],
                                                                                [Ast.JIdent ["int"]]))]) }
                                       ])
    let expected =
        let convert = "function ValueNone -> None | ValueSome x -> Some x"
        [| "static member ParseJson(v : UseData.Json.JsonValue) : Foo ="
           $"    {{ Baz = v |> UJson.fieldOpt \"Baz\" (UJson.map (UJson.nullable UJson.int |> %s{convert}))"
           "    }"
        |]
    let actual = JsonGenerator.run input
    Assert.That(actual, Is.EqualTo(expected))

[<Test>]
let ``union without associated data`` () =
    let input = Ast.Union ("Foo", [], [ { Name = "Bar"; Types = [] }
                                        { Name = "Baz"; Types = [] } ])
    let expected =
        [| "static member ParseJson(v : UseData.Json.JsonValue) : Foo ="
           "    match v |> UJson.field \"Case\" UJson.string with"
           "    | \"Bar\" -> Bar"
           "    | \"Baz\" -> Baz"
           "    | case -> failwithf \"Unrecognized case '%s' in union Foo\" case"
        |]
    let actual = JsonGenerator.run input
    Assert.That(actual, Is.EqualTo(expected))

[<Test>]
let ``union with associated data`` () =
    let input = Ast.Union ("Foo", [], [ { Name = "Bar"; Types = [] }
                                        { Name = "Baz"; Types = [Ast.JIdent ["int"]] } ])
    let expected =
        [| "static member ParseJson(v : UseData.Json.JsonValue) : Foo ="
           "    match v |> UJson.field \"Case\" UJson.string with"
           "    | \"Bar\" -> Bar"
           "    | \"Baz\" -> v |> UJson.field \"Fields\" (UJson.tuple1 UJson.int) |> Baz"
           "    | case -> failwithf \"Unrecognized case '%s' in union Foo\" case"
        |]
    let actual = JsonGenerator.run input
    Assert.That(actual, Is.EqualTo(expected))

[<Test>]
let ``generic union`` () =
    let input = Ast.Union ("Node", ["T"], [ { Name = "Leaf"; Types = [Ast.JVar "T"] }
                                            { Name = "Fork"
                                              Types =
                                                  let node = Ast.JApp (Ast.JIdent ["Node"], [Ast.JVar "T"])
                                                  [node; node]
                                            } ])
    let expected =
        let parsers = "parserForT : UseData.Json.JsonValue -> 'T"
        let nodeParser = "(Node.MakeJsonParser(parserForT))"
        [| $"static member MakeJsonParser(%s{parsers}) : UseData.Json.JsonValue -> Node = fun v ->"
           "    match v |> UJson.field \"Case\" UJson.string with"
           "    | \"Leaf\" -> v |> UJson.field \"Fields\" (UJson.tuple1 parserForT) |> Leaf"
           $"    | \"Fork\" -> v |> UJson.field \"Fields\" (UJson.tuple2 %s{nodeParser} %s{nodeParser}) |> Fork"
           "    | case -> failwithf \"Unrecognized case '%s' in union Node\" case"
        |]
    let actual = JsonGenerator.run input
    Assert.That(actual, Is.EqualTo(expected))
