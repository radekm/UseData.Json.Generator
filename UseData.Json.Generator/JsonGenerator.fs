module JsonGenerator

let run (simpleType : Ast.SimpleType) =
    match simpleType with
    | Ast.Record (name, fields) ->
        let fieldNames = fields |> Seq.map _.Name |> String.concat ", "
        [| $"// TODO: Generate deserializer for record with fields %s{fieldNames}."
           $"static member ParseJson(v : UseData.Json.JsonValue) : %s{name} = failwith \"TODO\"" |]
