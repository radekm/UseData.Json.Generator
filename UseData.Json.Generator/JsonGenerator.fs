module JsonGenerator

let parseSimpleType (longIdent : string list) =
    match longIdent with
    | ["string"] -> "UJson.string"
    // `UJson.dateTimeOffset` is unable to parse `DateTimeOffset` serialized by `System.Text.Json`.
    | ["DateTimeOffset"] ->
        let parse = "fun s -> DateTimeOffset.Parse(s, System.Globalization.CultureInfo.InvariantCulture)"
        $"(UJson.string >> %s{parse})"
    | ["int"] -> "UJson.int"
    | ["uint"] -> "UJson.uint"
    | ["int64"] -> "UJson.int64"
    | ["uint64"] -> "UJson.uint64"
    | ["decimal"] -> "UJson.decimal"
    | ["bool"] -> "UJson.bool"
    | x -> x |> String.concat "." |> fun tpe -> tpe + ".ParseJson"

let rec parseNonOptionType (t : Ast.JType) =
    match t with
    | Ast.JIdent longIdent -> parseSimpleType longIdent
    | Ast.JApp (Ast.JIdent ["list"], [t]) ->
        $"((UJson.map %s{parseAnyType t}) |> Array.toList)"
    | Ast.JArray (t) ->
        $"(UJson.map %s{parseAnyType t})"
    | _ -> failwithf "Unsupported non-option type: %A" t

and parseAnyType (t : Ast.JType) =
    match t with
    | Ast.JApp (Ast.JIdent ["voption"], [t]) -> $"(UJson.nullable %s{parseNonOptionType t})"
    | Ast.JApp (Ast.JIdent ["option"], [t]) ->
        let convert = "function ValueNone -> None | ValueSome x -> Some x"
        $"(UJson.nullable %s{parseNonOptionType t} |> %s{convert})"
    | _ -> parseNonOptionType t

let parseRecordField (field : Ast.RecordField) : string =
    match field.Type with
    | Ast.JApp (Ast.JIdent ["voption"], [t]) ->
        $"v |> UJson.fieldOpt \"%s{field.Name}\" %s{parseNonOptionType t}"
    | Ast.JApp (Ast.JIdent ["option"], [t]) ->
        let convert = "function ValueNone -> None | ValueSome x -> Some x"
        $"v |> UJson.fieldOpt \"%s{field.Name}\" %s{parseNonOptionType t} |> %s{convert}"
    | t -> $"v |> UJson.field \"%s{field.Name}\" %s{parseNonOptionType t}"

let generateRecordParser (fields : Ast.RecordField list) = seq {
    if fields.IsEmpty then
        failwith "Record has no fields"
    let mutable firstField = true
    for field in fields do
        let parse = parseRecordField field
        if firstField then
            yield $"    {{ %s{field.Name} = %s{parse}"
            firstField <- false
        else
            yield $"      %s{field.Name} = %s{parse}"
    yield "    }"
}

let parseUnionCase (case : Ast.UnionCase) : string =
    match case.Types with
    | [] -> case.Name  // Case has no associated data so there's no field `Fields` in JSON.
    | types ->
        let parseTuple =
            let parts = types |> List.map parseAnyType |> String.concat " "
            $"(UJson.tuple%d{types.Length} %s{parts})"
        $"v |> UJson.field \"Fields\" %s{parseTuple} |> %s{case.Name}"

let generateUnionParser (name : string) (cases : Ast.UnionCase list) = seq {
    if cases.IsEmpty then
        failwith "Union has no cases"
    yield "    match v |> UJson.field \"Case\" UJson.string with"
    for case in cases do
        yield $"    | \"%s{case.Name}\" -> %s{parseUnionCase case}"
    yield $"    | case -> failwithf \"Unrecognized case '%%s' in union %s{name}\" case"
}

let run (simpleType : Ast.SimpleType) =
    match simpleType with
    | Ast.Union (name, [], cases) ->
        Array.append
            [| $"static member ParseJson(v : UseData.Json.JsonValue) : %s{name} =" |]
            (generateUnionParser name cases |> Array.ofSeq)
    | Ast.Record (name, [], fields) ->
        Array.append
            [| $"static member ParseJson(v : UseData.Json.JsonValue) : %s{name} =" |]
            (generateRecordParser fields |> Array.ofSeq)
    | _ -> failwith "Type variables are currently not supported in generator"
