module JsonGenerator

let run (simpleType : Ast.SimpleType) =
    match simpleType with
    | Ast.Record fields ->
        let fieldNames = fields |> Seq.map _.Name |> String.concat ", "
        [| $"// TODO: Generate deserializer for record with fields %s{fieldNames}." |]
