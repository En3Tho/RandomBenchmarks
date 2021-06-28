open BenchmarkDotNet.Running
open FSharpBenchmarks
open FSharpBenchmarks.EnumeratorBenchmarks

[<EntryPoint>]
let main argv =
    BenchmarkRunner.Run<EnumeratorBenchmarks.EnumeratorBenchmarks>() |> ignore
    0