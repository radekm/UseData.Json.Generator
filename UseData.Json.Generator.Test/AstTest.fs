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
        Ast.Record ("Hello", [], [
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
        ])
    let actual =
        Ast.getUntypedTree checker "Temp.fsx" sourceText
        |> Ast.extractTypeInfo
    Assert.That(actual, Is.EqualTo(expected))

let ``input - union without associated data`` = """
type Hello =
    | Foo
    | Bar
"""

[<Test>]
let ``union cases are extracted - without associated data`` () =
    let checker = FSharpChecker.Create()
    let sourceText = SourceText.ofString ``input - union without associated data``
    let expected =
        Ast.Union ("Hello", [], [
            { Name = "Foo"; Types = [] }
            { Name = "Bar"; Types = [] }
        ])
    let actual =
        Ast.getUntypedTree checker "Temp.fsx" sourceText
        |> Ast.extractTypeInfo
    Assert.That(actual, Is.EqualTo(expected))

let ``input - union with associated data`` = """
type Node =
    | Leaf of int
    | Fork of Node * Node
"""

[<Test>]
let ``union cases are extracted - with associated data`` () =
    let checker = FSharpChecker.Create()
    let sourceText = SourceText.ofString ``input - union with associated data``
    let expected =
        Ast.Union ("Node", [], [
            { Name = "Leaf"; Types = [Ast.JIdent ["int"]] }
            { Name = "Fork"; Types = [Ast.JIdent ["Node"]; Ast.JIdent ["Node"]] }
        ])
    let actual =
        Ast.getUntypedTree checker "Temp.fsx" sourceText
        |> Ast.extractTypeInfo
    Assert.That(actual, Is.EqualTo(expected))

let ``input - union with associated data with labels`` = """
type Node =
    | Leaf of data:int
    | Fork of left:Node * right:Node
"""

[<Test>]
let ``union cases are extracted - with associated data with labels`` () =
    let checker = FSharpChecker.Create()
    let sourceText = SourceText.ofString ``input - union with associated data with labels``
    // Same result as without labels.
    let expected =
        Ast.Union ("Node", [], [
            { Name = "Leaf"; Types = [Ast.JIdent ["int"]] }
            { Name = "Fork"; Types = [Ast.JIdent ["Node"]; Ast.JIdent ["Node"]] }
        ])
    let actual =
        Ast.getUntypedTree checker "Temp.fsx" sourceText
        |> Ast.extractTypeInfo
    Assert.That(actual, Is.EqualTo(expected))

let ``input - generic union with associated data`` = """
type Node<'T> =
    | Leaf of 'T
    | Fork of Node<'T> * Node<'T>
"""

[<Test>]
let ``generic union cases are extracted - with associated data`` () =
    let checker = FSharpChecker.Create()
    let sourceText = SourceText.ofString ``input - generic union with associated data``
    let expected =
        Ast.Union ("Node", ["T"], [
            { Name = "Leaf"; Types = [Ast.JVar "T"] }
            { Name = "Fork"
              Types =
                  let node = Ast.JApp (Ast.JIdent ["Node"], [Ast.JVar "T"])
                  [node; node] }
        ])
    let actual =
        Ast.getUntypedTree checker "Temp.fsx" sourceText
        |> Ast.extractTypeInfo
    Assert.That(actual, Is.EqualTo(expected))

let ``input - generic record with two generic parameters`` = """
type KeyValue<'K, 'V> =
    { Key : 'K
      Value : 'V
    }
"""

[<Test>]
let ``generic record fields are extracted - with two generic parameters`` () =
    let checker = FSharpChecker.Create()
    let sourceText = SourceText.ofString ``input - generic record with two generic parameters``
    let expected =
        Ast.Record ("KeyValue", ["K"; "V"], [
            { Name = "Key"; Type = Ast.JVar "K" }
            { Name = "Value"; Type = Ast.JVar "V" }
        ])
    let actual =
        Ast.getUntypedTree checker "Temp.fsx" sourceText
        |> Ast.extractTypeInfo
    Assert.That(actual, Is.EqualTo(expected))

let ``input - union with single case without associated data`` = """
type Singleton = Singleton
"""

[<Test>]
let ``union cases are extracted - with single variant`` () =
    let checker = FSharpChecker.Create()
    let sourceText = SourceText.ofString ``input - union with single case without associated data``
    let expected =
        Ast.Union ("Singleton", [], [ { Name = "Singleton"; Types = [] } ])
    let actual =
        Ast.getUntypedTree checker "Temp.fsx" sourceText
        |> Ast.extractTypeInfo
    Assert.That(actual, Is.EqualTo(expected))


let ``input - generic union with single case without associated data`` = """
type Singleton<'T> = Singleton
"""

[<Test>]
let ``generic union cases are extracted - with single variant`` () =
    let checker = FSharpChecker.Create()
    let sourceText = SourceText.ofString ``input - generic union with single case without associated data``
    let expected =
        Ast.Union ("Singleton", ["T"], [ { Name = "Singleton"; Types = [] } ])
    let actual =
        Ast.getUntypedTree checker "Temp.fsx" sourceText
        |> Ast.extractTypeInfo
    Assert.That(actual, Is.EqualTo(expected))
