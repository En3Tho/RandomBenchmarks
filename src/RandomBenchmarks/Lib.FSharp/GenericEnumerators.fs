namespace Lib.FSharp.GenericEnumerators

open System.Collections.Generic
open System.Runtime.CompilerServices

module GenericEnumeratorsImpl =
    type SEnum<'i, 'e when 'e: struct
                   and 'e :> IEnumerator<'i>> = 'e

    [<Struct; NoComparison; NoEquality>]
    type GSelectEnumerator<'i, 'res, 'e when 'e: struct
                                         and 'e :> IEnumerator<'i>> =

        val mutable private enumerator: SEnum<'i, 'e>
        val private map: 'i -> 'res

        new (map, enumerator) = { enumerator = enumerator; map = map; }

        [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
        member this.MoveNext() = this.enumerator.MoveNext()
        member this.Current = this.enumerator.Current |> this.map

        member this.Dispose() = ()

        member this.GetEnumerator() = this

        interface IEnumerator<'res> with
            member this.Current = this.Current :> obj
            member this.Current = this.Current
            member this.Dispose() = ()
            member this.MoveNext() = this.MoveNext()
            member this.Reset() = ()

    [<Struct; NoComparison; NoEquality>]
    type GWhereEnumerator<'i, 'e when 'e: struct
                                  and 'e :> IEnumerator<'i>> =

        val mutable private enumerator: SEnum<'i, 'e>
        val private filter: 'i -> bool

        new (filter, enumerator) = { enumerator = enumerator; filter = filter; }

        [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
        member this.MoveNext() =
            let mutable found = false
            while not found && this.enumerator.MoveNext() do
                found <-
                    if this.enumerator.Current |> this.filter then
                        true
                    else
                        false
            found
        member this.Current = this.enumerator.Current

        member this.Dispose() = ()

        member this.GetEnumerator() = this

        interface IEnumerator<'i> with
            member this.Current = this.Current :> obj
            member this.Current = this.Current
            member this.Dispose() = ()
            member this.MoveNext() = this.MoveNext()
            member this.Reset() = ()

    [<Struct; NoComparison; NoEquality>]
    type ArrayEnumerator<'i> =
        val private array: 'i[]
        val mutable private index: int

        new (array) = { index = -1; array = array; }

        [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
        member this.MoveNext() =
            this.index <- this.index + 1
            uint32 this.index < uint32 this.array.Length
        member this.Current = this.array.[this.index]

        member this.Dispose() = ()

        member this.GetEnumerator() = this

        interface IEnumerator<'i> with
            member this.Current = this.Current :> obj
            member this.Current = this.Current
            member this.Dispose() = ()
            member this.MoveNext() = this.MoveNext()
            member this.Reset() = ()

module GSeq =
    open GenericEnumeratorsImpl
    let inline getEnumerator<'i, 'e, ^seq when 'e: struct
                                       and 'e :> IEnumerator<'i>
                                       and ^seq: (member GetEnumerator: unit -> SEnum<'i, 'e>)> (seq: ^seq) =
        (^seq: (member GetEnumerator: unit ->  SEnum<'i, 'e>) seq)

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    let ofArray array = new ArrayEnumerator<_>(array)

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    let filter filter enum = new GWhereEnumerator<_,_>(filter, enum)

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    let map map enum = new GSelectEnumerator<_,_,_>(map, enum)

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    let iter action (enum: SEnum<'i,'e>) =
        let mutable enum = enum
        while enum.MoveNext() do
            action enum.Current