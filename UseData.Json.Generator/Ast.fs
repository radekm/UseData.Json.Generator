module Ast

open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text

type JType =
    | JIdent of longIdent:string list
    | JVar of string
    | JApp of generic:JType * args:JType list
    | JArray of element:JType
    | JTuple of JType list

    static member FromSynType(t : SynType) =
        match t with
        | SynType.LongIdent (SynLongIdent (longIdent, _, _)) -> JIdent (longIdent |> List.map _.idText)
        | SynType.Var (SynTypar (ident, _, _), _) -> JVar ident.idText
        | SynType.App (generic, _, args, _, _, _, _) ->
            JApp (JType.FromSynType generic, args |> List.map JType.FromSynType)
        | SynType.Array (rank, element, _) ->
            if rank <> 1
            then failwith $"Only rank 1 arrays are supported: %A{t}"
            else JArray (JType.FromSynType element)
        // Only non-struct tuples.
        | SynType.Tuple (false, segments, _) ->
            segments
            |> List.choose (function
                | SynTupleTypeSegment.Type t -> Some (JType.FromSynType t)
                | SynTupleTypeSegment.Star _ -> None  // Skip stars between type names.
                | segment -> failwith $"Unsupported tuple segment: %A{segment}")
            |> JType.JTuple
        | _ -> failwith $"Unsupported type: %A{t}"

type UnionCase = { Name : string
                   Types : JType list }

type EnumCase = { Name : string }

type RecordField = { Name : string
                     Type : JType }

type SimpleType =
    | Union of name:string * typeVars:string list * cases:UnionCase list
    // TODO: Handle enums.
    // | Enum of EnumCase list
    | Record of name:string * typeVars:string list * fields:RecordField list

/// `input` is a parsed file containing exactly a single type.
let extractTypeInfo (input : ParsedInput) : SimpleType =
    match input with
    | ParsedInput.ImplFile x ->
        match x.Contents with
        | [SynModuleOrNamespace (_, _, _, [SynModuleDecl.Types ([typeDefn], _)], _, _, _, _, _)] ->
            match typeDefn with
            | SynTypeDefn (typeInfo, SynTypeDefnRepr.Simple (simpleRepr, _), _, _, _, _) ->
                let name, typeVars =
                    match typeInfo with
                    | SynComponentInfo (_, typeVars, _, [name], _, _, _, _) ->
                        let typeVars =
                            match typeVars with
                            | None -> []
                            | Some (SynTyparDecls.PostfixList (decls, _, _)) ->
                                decls
                                |> List.map (fun (SynTyparDecl (_, SynTypar (ident, _, _), _, _)) -> ident.idText)
                            | Some _ -> failwithf "Unsupported form of type variable declaration: %A" typeVars
                        name.idText, typeVars
                    | _ -> failwithf "Expected type name with single component: %A" typeInfo
                match simpleRepr with
                | SynTypeDefnSimpleRepr.Union (_, unionCases, _) ->
                    unionCases
                    |> List.map (function
                        | SynUnionCase (_, SynIdent (ident, None), SynUnionCaseKind.Fields fields, _, _, _, _) ->
                            let types =
                                fields
                                |> List.map (function
                                    | SynField (_, _, _, tpe, _, _, _, _, _) -> JType.FromSynType tpe)
                            { Name = ident.idText; Types = types }
                        | case -> failwithf "Union cases like this are not supported: %A" case)
                    |> fun cases -> Union (name, typeVars, cases)
                // Unions with single case without associated data are special.
                | SynTypeDefnSimpleRepr.TypeAbbrev (_, SynType.LongIdent (SynLongIdent ([case], _, _)), _) ->
                    Union (name, typeVars, [ { Name = case.idText; Types = [] } ])
                | SynTypeDefnSimpleRepr.Enum (enumCases, _) ->
                    failwithf "Enums are not yet implemented: %A" enumCases
                | SynTypeDefnSimpleRepr.Record (_, recordFields, _) ->
                    recordFields
                    |> List.map (function
                        | SynField (_, _, name, tpe, _, _, _, _, _) ->
                            match name with
                            | None -> failwith "Record field without name"
                            | Some name -> { Name = name.idText; Type = JType.FromSynType tpe })
                    |> fun fields -> Record (name, typeVars, fields)
                | x -> failwithf "Expected union or enum or record: %A" x
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
