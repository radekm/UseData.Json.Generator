module JsonGenerator

let parseSingleValue (longIdent : string list) =
    match longIdent with
    | ["string"] -> "UJson.string"
    | ["DateTimeOffset"] -> "UJson.dateTimeOffset"
    | ["int"] -> "UJson.int"
    | ["uint"] -> "UJson.uint"
    | ["int64"] -> "UJson.int64"
    | ["uint64"] -> "UJson.uint64"
    | ["decimal"] -> "UJson.decimal"
    | ["bool"] -> "UJson.bool"
    | x -> x |> String.concat "." |> fun tpe -> tpe + ".ParseJson"

let parseRecordField (field : Ast.RecordField) : string =
    match field.Type with
    | Ast.JIdent longIdent -> $"v |> UJson.field \"%s{field.Name}\" %s{parseSingleValue longIdent}"
    | Ast.JApp (Ast.JIdent ["voption"], [Ast.JIdent longIdent]) ->
        $"v |> UJson.fieldOpt \"%s{field.Name}\" %s{parseSingleValue longIdent}"
    | Ast.JApp (Ast.JIdent ["option"], [Ast.JIdent longIdent]) ->
        let convert = "function ValueNone -> None | ValueSome x -> Some x"
        $"v |> UJson.fieldOpt \"%s{field.Name}\" %s{parseSingleValue longIdent} |> %s{convert}"
    | Ast.JApp (Ast.JIdent ["list"], [Ast.JIdent longIdent]) ->
        $"v |> UJson.field \"%s{field.Name}\" (UJson.map %s{parseSingleValue longIdent}) |> Array.toList"
    | Ast.JArray (Ast.JIdent longIdent) ->
        $"v |> UJson.field \"%s{field.Name}\" (UJson.map %s{parseSingleValue longIdent})"
    | _ -> failwithf "Field %s has unsupported type: %A" field.Name field.Type

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

let run (simpleType : Ast.SimpleType) =
    match simpleType with
    | Ast.Record (name, fields) ->
        Array.append
            [| $"static member ParseJson(v : UseData.Json.JsonValue) : %s{name} =" |]
            (generateRecordParser fields |> Array.ofSeq)
