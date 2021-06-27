namespace FSharpBenchmarks.EnumeratorBenchmarks

open System
open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Jobs

open Lib.FSharp.GenericEnumerators
open Lib.FSharp.InlineEnumerators
open Lib.FSharp.ValueOptionEnumerators

module Helpers =
    let createArray() = [|
       for i = 0 to 3 do
            for j = 0 to 99 do
                $"{i}, {j}"
    |]

[<DisassemblyDiagnoser>]
[<MemoryDiagnoser>]
[<SimpleJob(RuntimeMoniker.Net50)>]
[<SimpleJob(RuntimeMoniker.Net60)>]
type EnumeratorBenchmarks() =

    let array = Helpers.createArray()
    let list = ResizeArray array

    let containsZero = id (fun (str: string) -> str.Contains "0")
    let containsOne = id (fun (str: string) -> str.Contains "1")
    let containsTwo = id (fun (str: string) -> str.Contains "2")
    let ignore = id (fun (str: string) -> if str.Length = 0 then Console.Write str)

    let valuefst struct (x, _) = x
    let valuesnd struct (_, x) = x

    let valueStrLen struct (_, x: string) = x.Length
    let strLen (_, x: string) = x.Length

    [<Benchmark>]
    member _.SeqArrayIndexedMinBy() =
        array
        |> Seq.indexed
        |> Seq.minBy strLen
        |> fst

    [<Benchmark>]
    member _.ISeqArrayRefIndexedMinBy() =
        array
        |> ISeq.ofArray
        |> ISeq.refIndexed
        |> ISeq.minBy strLen
        |> fst

    [<Benchmark>]
    member _.VOSeqArrayRefIndexedMinBy() =
        array
        |> VOSeq.ofArray
        |> VOSeq.refIndexed
        |> VOSeq.minBy strLen
        |> fst

    [<Benchmark>]
    member _.ISeqArrayIndexedMinBy() =
        array
        |> ISeq.ofArray
        |> ISeq.indexed
        |> ISeq.minBy valueStrLen
        |> valuefst

    [<Benchmark>]
    member _.VOSeqArrayIndexedMinBy() =
        array
        |> VOSeq.ofArray
        |> VOSeq.indexed
        |> VOSeq.minBy valueStrLen
        |> valuefst
  
    [<Benchmark>]
    member _.ExplicitArrayFind() =
        let mutable value = array.[0]
        let mutable resultIndex = 0
        for i = 1 to array.Length - 1 do
            if array.[i].Length < value.Length then
                value <- array.[i]
                resultIndex <- i
        resultIndex

    [<Benchmark>]
    member _.SeqListFilterIter() =
        list
        |> Seq.filter containsZero
        |> Seq.iter ignore

    [<Benchmark>]
    member _.SeqListFilterFilterIter() =
        list
        |> Seq.filter containsOne
        |> Seq.filter containsZero
        |> Seq.iter ignore

    [<Benchmark>]
    member _.ISeqListFilterIter() =
        list
        |> ISeq.filter containsZero
        |> ISeq.iter ignore

    [<Benchmark>]
    member _.ISeqListFilterFilterIter() =
        list
        |> ISeq.filter containsOne
        |> ISeq.filter containsZero
        |> ISeq.iter ignore

    [<Benchmark>]
    member _.ISeqListFilter2Iter() =
        list
        |> ISeq.filter2 containsZero
        |> ISeq.iter ignore

    [<Benchmark>]
    member _.ISeqLustFilter2Filter2Iter() =
        list
        |> ISeq.filter2 containsOne
        |> ISeq.filter2 containsZero
        |> ISeq.iter ignore

    [<Benchmark>]
    member _.GSeqListFilterIter() =
        list
        |> GSeq.getEnumerator
        |> GSeq.filter containsZero
        |> GSeq.iter ignore

    [<Benchmark>]
    member _.GSeqFilterFilterIter() =
        list
        |> GSeq.getEnumerator
        |> GSeq.filter containsOne
        |> GSeq.filter containsZero
        |> GSeq.iter ignore

    [<Benchmark>]
    member _.ExplicitListFilterFilterIter() =
        let mutable enumerator = list.GetEnumerator()
        while enumerator.MoveNext() do
        if containsOne enumerator.Current
           && containsZero enumerator.Current then
               ignore enumerator.Current

    [<Benchmark>]
    member _.SeqArrayFilterIter() =
        array
        |> Seq.filter containsZero
        |> Seq.iter ignore

    [<Benchmark>]
    member _.SeqArrayFilterFilterIter() =
        array
        |> Seq.filter containsOne
        |> Seq.filter containsZero
        |> Seq.iter ignore

    [<Benchmark>]
    member _.ISeqArrayFilterIter() =
        array
        |> ISeq.ofArray
        |> ISeq.filter containsZero
        |> ISeq.iter ignore

    [<Benchmark>]
    member _.VOSeqArrayFilterIter() =
        array
        |> VOSeq.ofArray
        |> VOSeq.filter containsZero
        |> VOSeq.iter ignore

    [<Benchmark>]
    member _.ISeqArrayFilterFilterIter() =
        array
        |> ISeq.ofArray
        |> ISeq.filter containsOne
        |> ISeq.filter containsZero
        |> ISeq.iter ignore

    [<Benchmark>]
    member _.VOSeqArrayFilterFilterIter() =
        array
        |> VOSeq.ofArray
        |> VOSeq.filter containsOne
        |> VOSeq.filter containsZero
        |> VOSeq.iter ignore

    [<Benchmark>]
    member _.ISeqArrayFilterFilterFilterIter() =
        array
        |> ISeq.ofArray
        |> ISeq.filter containsOne
        |> ISeq.filter containsZero
        |> ISeq.filter containsTwo
        |> ISeq.iter ignore

    [<Benchmark>]
    member _.VOSeqArrayFilterFilterFilterIter() =
        array
        |> VOSeq.ofArray
        |> VOSeq.filter containsOne
        |> VOSeq.filter containsZero
        |> VOSeq.filter containsTwo
        |> VOSeq.iter ignore

    [<Benchmark>]
    member _.ISeqArrayFilter2Iter() =
        array
        |> ISeq.ofArray
        |> ISeq.filter2 containsZero
        |> ISeq.iter ignore

    [<Benchmark>]
    member _.VOSeqArrayFilter2Iter() =
        array
        |> VOSeq.ofArray
        |> VOSeq.filter2 containsZero
        |> VOSeq.iter ignore

    [<Benchmark>]
    member _.ISeqArrayFilter2Filter2Iter() =
        array
        |> ISeq.ofArray
        |> ISeq.filter2 containsOne
        |> ISeq.filter2 containsZero
        |> ISeq.iter ignore

    [<Benchmark>]
    member _.VOSeqArrayFilter2Filter2Iter() =
        array
        |> VOSeq.ofArray
        |> VOSeq.filter2 containsOne
        |> VOSeq.filter2 containsZero
        |> VOSeq.iter ignore

    [<Benchmark>]
    member _.ISeqArrayFilter2Filter2Filter2Iter() =
        array
        |> ISeq.ofArray
        |> ISeq.filter2 containsOne
        |> ISeq.filter2 containsZero
        |> ISeq.filter2 containsTwo
        |> ISeq.iter ignore

    [<Benchmark>]
    member _.VOSeqArrayFilter2Filter2Filter2Iter() =
        array
        |> VOSeq.ofArray
        |> VOSeq.filter2 containsOne
        |> VOSeq.filter2 containsZero
        |> VOSeq.filter2 containsTwo
        |> VOSeq.iter ignore

    [<Benchmark>]
    member _.GSeqArrayFilterIter() =
        array
        |> GSeq.ofArray
        |> GSeq.filter containsZero
        |> GSeq.iter ignore

    [<Benchmark>]
    member _.GSeqArrayFilterFilterIter() =
        array
        |> GSeq.ofArray
        |> GSeq.filter containsOne
        |> GSeq.filter containsZero
        |> GSeq.iter ignore

    [<Benchmark>]
    member _.GSeqArrayFilterFilterFilterIter() =
        array
        |> GSeq.ofArray
        |> GSeq.filter containsOne
        |> GSeq.filter containsZero
        |> GSeq.filter containsTwo
        |> GSeq.iter ignore

    [<Benchmark>]
    member _.ExplicitArrayFilterFilterIter() =
        for i in array do
            if containsOne i && containsZero i then ignore i

    [<Benchmark>]
    member _.ExplicitArrayFilterFilterIter_2() =
        for i = 0 to array.Length - 1 do
            let i = array.[i]
            if containsOne i && containsZero i then ignore i

    [<Benchmark>]
    member _.ExplicitGSeqArrayFilterFilterIter() =
        let mutable enum = array |> GSeq.ofArray
        while enum.MoveNext() do
            let i = enum.Current
            if containsOne i && containsZero i then ignore i