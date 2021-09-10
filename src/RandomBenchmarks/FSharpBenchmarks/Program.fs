open BenchmarkDotNet.Running
open FSharpBenchmarks
open FSharpBenchmarks.EnumeratorBenchmarks

[<EntryPoint>]
let main argv =
    BenchmarkRunner.Run<EnumeratorFoldMinimumReproBenchmarks>() |> ignore
    0