namespace FSharpBenchmarks.EnumeratorBenchmarks

open System.Collections.Generic
open System.Runtime.CompilerServices
open BenchmarkDotNet.Attributes
open Lib.CSharp
open Lib.FSharp

module GSeq =
    type SStructEnumerator<'i, 'e when 'e: struct
                       and 'e :> IEnumerator<'i>> = 'e

    let inline getEnumerator<'i, 'e, ^seq when 'e: struct
                                       and 'e :> IEnumerator<'i>
                                       and ^seq: (member GetEnumerator: unit -> SStructEnumerator<'i, 'e>)> (seq: ^seq) =
        (^seq: (member GetEnumerator: unit ->  SStructEnumerator<'i, 'e>) seq)

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    let fold initial folder (enumerator: SStructEnumerator<'i,'e>) =
        let folder = OptimizedClosures.FSharpFunc<_,_,_>.Adapt folder
        let mutable enumerator = enumerator
        let mutable result = initial
        while enumerator.MoveNext() do
            result <- folder.Invoke(result, enumerator.Current)
        result

[<DisassemblyDiagnoser; MemoryDiagnoser>]
[<Config(typeof<BenchmarkConfig>)>]
type EnumeratorFoldMinimumReproBenchmarks() =

    [<Params(100)>]
    member val Count = 0 with get, set
    member val List = ResizeArray() with get, set

    [<GlobalSetup>]
    member this.GlobalSetup() =
        this.List <- ResizeArray (Array.init this.Count id)

    [<Benchmark>]
    member this.ListGSeqFold() =
        this.List
        |> GSeq.getEnumerator
        |> GSeq.fold 0 ^ fun x y -> x + y

    [<Benchmark>]
    member this.ListCSharpFold() =
        this.List
        |> GSeq.getEnumerator
        |> fun x -> GSeqCSharp.Fold(0, (fun x y -> x + y), x) // inlined, so don't care