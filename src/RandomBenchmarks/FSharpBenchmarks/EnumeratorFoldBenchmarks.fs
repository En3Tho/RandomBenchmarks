namespace FSharpBenchmarks.EnumeratorBenchmarks

open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Configs
open BenchmarkDotNet.Environments
open BenchmarkDotNet.Jobs
open Lib.CSharp
open Lib.FSharp
open Lib.FSharp.GenericEnumerators
open Lib.FSharp.InlineEnumerators

type private BenchmarkConfig() as this =
    inherit ManualConfig()

    let [<Literal>] TieredPGO = "DOTNET_TieredPGO"
    let [<Literal>] QuickJitForLoops = "DOTNET_TC_QuickJitForLoop"
    let [<Literal>] ReadyToRun = "DOTNET_ReadyToRun"

    do
        this.AddJob([|
//            Job.Default.WithRuntime(CoreRuntime.Core50).WithId("Net5")
            Job.Default.WithRuntime(CoreRuntime.Core60).WithId("NoPGO")
//            Job.Default.WithRuntime(CoreRuntime.Core60).WithId("DynamicPGO")
//                        .WithEnvironmentVariables(EnvironmentVariable(TieredPGO, "1"),
//                                                  EnvironmentVariable(QuickJitForLoops, "1"),
//                                                  EnvironmentVariable(ReadyToRun, "0"))
        |]) |> ignore

[<DisassemblyDiagnoser; MemoryDiagnoser>]
[<Config(typeof<BenchmarkConfig>)>]
type EnumeratorFoldBenchmarks() =

    [<Params(10, 100, 1000)>]
    member val Count = 0 with get, set

    member val Array = [||] with get, set
    member val List = ResizeArray() with get, set

    [<GlobalSetup>]
    member this.GlobalSetup() =
        this.Array <- Array.init this.Count id
        this.List <- ResizeArray this.Array

    [<Benchmark>]
    member this.ListISeqFold() =
        this.List
        |> ISeq.fold 0 ^ fun x y -> x + y

    [<Benchmark>]
    member this.ListISeqFoldTupled() =
        this.List
        |> fun x -> ISeq.foldTupled(0, (fun x y -> x + y), x)

    [<Benchmark>]
    member this.ListISeq2FoldTupled() =
        this.List
        |> fun x -> ISeq.ISeq2.FoldTupled(0, (fun x y -> x + y), x)

    [<Benchmark>]
    member this.ListGSeqFold() =
        this.List
        |> GSeq.getEnumerator
        |> GSeq.fold 0 ^ fun x y -> x + y

    [<Benchmark>]
    member this.ListGSeqFoldTupled() =
        this.List
        |> GSeq.getEnumerator
        |> fun x -> GSeq.FoldTupled(0, (fun x y -> x + y), x)

    [<Benchmark>]
    member this.ListGSeq2FoldTupled() =
        this.List
        |> GSeq.getEnumerator
        |> fun x -> GSeq.GSeq2.FoldTupled(0, (fun x y -> x + y), x)

    [<Benchmark>]
    member this.ListCSharpFold() =
        this.List
        |> GSeq.getEnumerator
        |> fun x -> GSeqCSharp.Fold(0, (fun x y -> x + y), x)