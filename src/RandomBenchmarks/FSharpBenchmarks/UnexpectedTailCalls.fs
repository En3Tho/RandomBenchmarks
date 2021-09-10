module Tailcalls

open System.Collections.Generic
open System.Runtime.CompilerServices
open BenchmarkDotNet.Attributes

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

let noTailCalls (list: ResizeArray<_>) =
    list
    |> GSeq.getEnumerator
    |> GSeq.fold 0 (fun x y -> x + y)

let whyTailCalls (list: ResizeArray<_>) =
    list
    |> Lib.FSharp.GenericEnumerators.GSeq.getEnumerator
    |> Lib.FSharp.GenericEnumerators.GSeq.fold 0 (fun x y -> x + y)

[<DisassemblyDiagnoser; MemoryDiagnoser>]
type WhyTailCalls() =
    member val List = ResizeArray() with get, set

    member _.WhyTailCalls(list: ResizeArray<int>) =
            list
            |> Lib.FSharp.GenericEnumerators.GSeq.getEnumerator
            |> Lib.FSharp.GenericEnumerators.GSeq.fold 0 (fun x y -> x + y)

    [<Benchmark>]
    member this.NoTailCalls() =
        this.List
        |> GSeq.getEnumerator
        |> GSeq.fold 0 (fun x y -> x + y)