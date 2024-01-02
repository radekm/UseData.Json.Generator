open System
open System.IO

open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Text

let processFile (checker : FSharpChecker) (path : string) =
    let processTypeDeclaration =
        GenericGenerator.processTypeDeclaration (Ast.extractTypeInfo >> JsonGenerator.run) checker

    let output = ResizeArray<string>()
    let input = File.ReadAllText path
    let sourceText = SourceText.ofString input
    let changed = GenericGenerator.run sourceText output.Add processTypeDeclaration

    if changed then
        printfn "Updating file %s" path
        // `File.WriteAllLines` adds additional new line at the end of file.
        File.WriteAllText(path, String.concat "\n" output)

[<EntryPoint>]
let main args =
    match args with
    | [| dir |] ->
        if not (Directory.Exists dir) then
           failwith $"Directory doesn't exist: %s{dir}"
        let checker = FSharpChecker.Create()
        let sourceFiles = Directory.GetFiles(dir, "*.fs") |> Array.sort
        for sourceFile in sourceFiles do
            printfn "Processing file %s" sourceFile
            processFile checker sourceFile
    | _ -> failwith "Exactly one directory must be given as an argument"

    0
