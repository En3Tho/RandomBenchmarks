open BenchmarkDotNet.Running
open FSharpBenchmarks.EnumeratorBenchmarks

[<EntryPoint>]
let main argv =
    BenchmarkRunner.Run<EnumeratorBenchmarks>() |> ignore
    0