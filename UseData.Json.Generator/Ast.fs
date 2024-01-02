module Ast

open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text

type JType =
    | JIdent of longIdent:string list
    | JApp of generic:JType * args:JType list
    | JArray of element:JType

    static member FromSynType(t : SynType) =
        match t with
        | SynType.LongIdent (SynLongIdent (longIdent, _, _)) -> JIdent (longIdent |> List.map _.idText)
        | SynType.App (generic, _, args, _, _, _, _) ->
            JApp (JType.FromSynType generic, args |> List.map JType.FromSynType)
        | SynType.Array (rank, element, _) ->
            if rank <> 1
            then failwith $"Only rank 1 arrays are supported: %A{t}"
            else JArray (JType.FromSynType element)
        | _ -> failwith $"Unsupported type: %A{t}"

type UnionCase = { Name : string
                   Types : JType list }

type EnumCase = { Name : string }

type RecordField = { Name : string
                     Type : JType }

type SimpleType =
    // TODO: Handle unions and enums.
    // | Union of UnionCase list
    // | Enum of EnumCase list
    | Record of name:string * fields:RecordField list

/// `input` is a parsed file containing exactly a single type.
let extractTypeInfo (input : ParsedInput) : SimpleType =
    match input with
    | ParsedInput.ImplFile x ->
        match x.Contents with
        | [SynModuleOrNamespace (_, _, _, [SynModuleDecl.Types ([typeDefn], _)], _, _, _, _, _)] ->
            match typeDefn with
            | SynTypeDefn (typeInfo, SynTypeDefnRepr.Simple (simpleRepr, _), _, _, _, _) ->
                let name =
                    match typeInfo with
                    | SynComponentInfo (_, _, _, [name], _, _, _, _) -> name.idText
                    | _ -> failwithf "Expected type name with single component: %A" typeInfo
                match simpleRepr with
                | SynTypeDefnSimpleRepr.Union(_, unionCases, _) ->
                    failwithf "Unions are not yet implemented: %A" unionCases
                | SynTypeDefnSimpleRepr.Enum(enumCases, _) ->
                    failwithf "Enums are not yet implemented: %A" enumCases
                | SynTypeDefnSimpleRepr.Record (_, recordFields, _) ->
                    recordFields
                    |> List.map (function
                        | SynField (_, _, name, tpe, _, _, _, _, _) ->
                            match name with
                            | None -> failwith "Record field without name"
                            | Some name -> { Name = name.idText; Type = JType.FromSynType tpe })
                    |> fun fields -> Record (name, fields)
                | _ -> failwith "Expected union or enum or record"
            | _ -> failwith "Expected simple type"
        | _ -> failwith "Expected single type declaration"
    | ParsedInput.SigFile _ -> failwith "Expected implementation file not signature file"

let getUntypedTree (checker : FSharpChecker) (file : string) (sourceText : ISourceText) =
    // Get compiler options for the 'project' implied by a single script file.
    let projOptions, _ =
        checker.GetProjectOptionsFromScript(file, sourceText, assumeDotNetFramework = false)
        |> Async.RunSynchronously

    let parsingOptions, _errors = checker.GetParsingOptionsFromProjectOptions(projOptions)

    // Run the first phase (untyped parsing) of the compiler.
    let parseFileResults =
        checker.ParseFile(file, sourceText, parsingOptions)
        |> Async.RunSynchronously

    parseFileResults.ParseTree
