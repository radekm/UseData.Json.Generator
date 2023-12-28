module AstTest

open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Text
open NUnit.Framework

let ``input - record`` = """
type Hello =
    { Foo : int
      Bar : string
      OneParam : ResizeArray<string>
      TwoParams : Dictionary<int, string>
      Nested : array<int option list>
      Array : int[]
    }
"""

[<Test>]
let ``record fields are extracted`` () =
    let checker = FSharpChecker.Create()
    let sourceText = SourceText.ofString ``input - record``
    let expected =
        Ast.Record [
            { Name = "Foo"; Type = Ast.JIdent ["int"] }
            { Name = "Bar"; Type = Ast.JIdent ["string"] }
            { Name = "OneParam"; Type = Ast.JApp (Ast.JIdent ["ResizeArray"], [Ast.JIdent ["string"]]) }
            { Name = "TwoParams"; Type = Ast.JApp (Ast.JIdent ["Dictionary"], [Ast.JIdent ["int"]
                                                                               Ast.JIdent ["string"]]) }
            { Name = "Nested"
              Type = Ast.JApp (Ast.JIdent ["array"],
                               [Ast.JApp (Ast.JIdent ["list"],
                                          [Ast.JApp (Ast.JIdent ["option"], [Ast.JIdent ["int"]])])]) }
            { Name = "Array"; Type = Ast.JArray (Ast.JIdent ["int"]) }
        ]
    let actual =
        Ast.getUntypedTree checker "Temp.fsx" sourceText
        |> Ast.extractFieldsOrCases
    Assert.That(actual, Is.EqualTo(expected))
