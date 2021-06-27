module FSharpBenchmarks.NestedFuncCodeGen

open System.Collections.Generic
open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Jobs

let identicalBy valueEqualsTo (seq2: 'a seq) (seq1: 'a seq) =
    use enum1 = seq1.GetEnumerator()
    use enum2 = seq2.GetEnumerator()

    // compiles to method
    let rec moveNext() =
        match enum1.MoveNext(), enum2.MoveNext() with
        | true, true ->
            enum1.Current |> valueEqualsTo enum2.Current
            && moveNext()
        | false, false -> true
        | _ -> false

    moveNext()

let identicalByCurried valueEqualsTo (seq2: 'a seq) (seq1: 'a seq) =
    use enum1 = seq1.GetEnumerator()
    use enum2 = seq2.GetEnumerator()

    // compiles to lambda and creates a new one with every iteration
    let rec moveNext valueEqualsTo (enum2: 'a IEnumerator) (enum1: 'a IEnumerator) =
        match enum1.MoveNext(), enum2.MoveNext() with
        | true, true ->
            enum1.Current |> valueEqualsTo enum2.Current
            && enum1 |> moveNext valueEqualsTo enum2
        | false, false -> true
        | _ -> false

    enum1 |> moveNext valueEqualsTo enum2

let identicalByTupled valueEqualsTo (seq2: 'a seq) (seq1: 'a seq) =
    use enum1 = seq1.GetEnumerator()
    use enum2 = seq2.GetEnumerator()

    // compiles to method
    let rec moveNext(valueEqualsTo, enum2: 'a IEnumerator, enum1: 'a IEnumerator) =
        match enum1.MoveNext(), enum2.MoveNext() with
        | true, true ->
            enum1.Current |> valueEqualsTo enum2.Current
            && moveNext(valueEqualsTo, enum2, enum1)
        | false, false -> true
        | _ -> false

    moveNext(valueEqualsTo, enum2, enum1)

[<DisassemblyDiagnoser>]
[<MemoryDiagnoser>]
[<SimpleJob(RuntimeMoniker.Net50)>]
type Benchmark() =
    let shorterOne = ResizeArray([| 1; 2; 3; 4; 5; 6; 7; 8; 9; 10 |])
    let longerOne = ResizeArray([| 1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11 |])

    [<Benchmark>]
    member _.IdenticalByImplicitCodeGen() = shorterOne |> identicalBy (=) longerOne

    [<Benchmark>]
    member _.IdenticalByExplicitCurredForm() = shorterOne |> identicalByCurried (=) longerOne

    [<Benchmark>]
    member _.IdenticalByExplicitTupledForm() = shorterOne |> identicalByTupled (=) longerOne