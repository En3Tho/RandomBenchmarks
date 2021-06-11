namespace FSharpBenchmarks.EnumeratorBenchmarks

open System
open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Jobs

open Lib.FSharp.GenericEnumerators
open Lib.FSharp.InlineEnumerators

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
    member _.ArrayFind() =
        array
        |> Seq.indexed
        |> Seq.minBy strLen
        |> fst

    [<Benchmark>]
    member _.SArrayFind() =
        array
        |> ISeq.ofArray
        |> ISeq.refIndexed
        |> ISeq.minBy strLen
        |> fst

    [<Benchmark>]
    member _.SValueArrayFindExplicit() =
        array
        |> ISeq.ofArray
        |> ISeq.indexed
        |> ISeq.minBy valueStrLen
        |> valuefst

    [<Benchmark>]
    member _.SValueArrayFind() =
        array
        |> ISeq.ofArray
        |> ISeq.indexed
        |> ISeq.minBy valueStrLen
        |> valuefst
  
    [<Benchmark>]
    member _.ForArrayFind() =
        let mutable value = array.[0]
        let mutable resultIndex = 0
        for i = 1 to array.Length - 1 do
            if array.[i].Length < value.Length then
                value <- array.[i]
                resultIndex <- i
        resultIndex

    [<Benchmark>]
    member _.CListSeqIter1() =
        list
        |> Seq.filter containsZero
        |> Seq.iter ignore

    [<Benchmark>]
    member _.CListSeqIter2() =
        list
        |> Seq.filter containsOne
        |> Seq.filter containsZero
        |> Seq.iter ignore

    [<Benchmark>]
    member _.CListSSeqIter1() =
        list
        |> ISeq.filter containsZero
        |> ISeq.iter ignore

    [<Benchmark>]
    member _.CListSSeqIter2() =
        list
        |> ISeq.filter containsOne
        |> ISeq.filter containsZero
        |> ISeq.iter ignore

    [<Benchmark>]
    member _.CListSSeqIter12() =
        list
        |> ISeq.filter2 containsZero
        |> ISeq.iter ignore

    [<Benchmark>]
    member _.CListSSeqIter22() =
        list
        |> ISeq.filter2 containsOne
        |> ISeq.filter2 containsZero
        |> ISeq.iter ignore

    [<Benchmark>]
    member _.CListGSeqIter1() =
        list
        |> GSeq.getEnumerator
        |> GSeq.filter containsZero
        |> GSeq.iter ignore

    [<Benchmark>]
    member _.CListGSeqIter2() =
        list
        |> GSeq.getEnumerator
        |> GSeq.filter containsOne
        |> GSeq.filter containsZero
        |> GSeq.iter ignore

    [<Benchmark>]
    member _.CListRawIter1() =
        let mutable enumerator = list.GetEnumerator()
        while enumerator.MoveNext() do
        if containsOne enumerator.Current
           && containsZero enumerator.Current then
               ignore enumerator.Current

    [<Benchmark>]
    member _.ArraySeqIter1() =
        array
        |> Seq.filter containsZero
        |> Seq.iter ignore

    [<Benchmark>]
    member _.ArraySeqIter2() =
        array
        |> Seq.filter containsOne
        |> Seq.filter containsZero
        |> Seq.iter ignore

    [<Benchmark>]
    member _.ArrayISeqIter1() =
        array
        |> ISeq.ofArray
        |> ISeq.filter containsZero
        |> ISeq.iter ignore

    [<Benchmark>]
    member _.ArrayISeqIter2() =
        array
        |> ISeq.ofArray
        |> ISeq.filter containsOne
        |> ISeq.filter containsZero
        |> ISeq.iter ignore

    [<Benchmark>]
    member _.ArrayISeqIter3() =
        array
        |> ISeq.ofArray
        |> ISeq.filter containsOne
        |> ISeq.filter containsZero
        |> ISeq.filter containsTwo
        |> ISeq.iter ignore

    [<Benchmark>]
    member _.ArrayISeqIter12() =
        array
        |> ISeq.ofArray
        |> ISeq.filter2 containsZero
        |> ISeq.iter ignore

    [<Benchmark>]
    member _.ArrayISeqIter22() =
        array
        |> ISeq.ofArray
        |> ISeq.filter2 containsOne
        |> ISeq.filter2 containsZero
        |> ISeq.iter ignore

    [<Benchmark>]
    member _.ArrayISeqIter32() =
        array
        |> ISeq.ofArray
        |> ISeq.filter2 containsOne
        |> ISeq.filter2 containsZero
        |> ISeq.filter2 containsTwo
        |> ISeq.iter ignore

    [<Benchmark>]
    member _.ArrayGSeqIter1() =
        array
        |> GSeq.ofArray
        |> GSeq.filter containsZero
        |> GSeq.iter ignore

    [<Benchmark>]
    member _.ArrayGSeqIter2() =
        array
        |> GSeq.ofArray
        |> GSeq.filter containsOne
        |> GSeq.filter containsZero
        |> GSeq.iter ignore

    [<Benchmark>]
    member _.ArrayGSeqIter3() =
        array
        |> GSeq.ofArray
        |> GSeq.filter containsOne
        |> GSeq.filter containsZero
        |> GSeq.filter containsTwo
        |> GSeq.iter ignore

    [<Benchmark>]
    member _.ArrayFor() =
        for i in array do
            if containsOne i && containsZero i then ignore i

    [<Benchmark>]
    member _.ArrayFor2() =
        for i = 0 to array.Length - 1 do
            let i = array.[i]
            if containsOne i && containsZero i then ignore i

    [<Benchmark>]
    member _.ArrayEnumeratorFor() =
        let mutable enum = array |> GSeq.ofArray
        while enum.MoveNext() do
            let i = enum.Current
            if containsOne i && containsZero i then ignore i