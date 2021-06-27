namespace Lib.FSharp.InlineEnumerators

open System.Collections.Generic
open Lib.FSharp.Core

module InlineShapeEnumeratorsImpl =
    type SEnumerator< ^a, ^b when ^a: struct
                          and ^a: (member Current: ^b)
                          and ^a: (member MoveNext: unit -> bool)> = ^a

    module SEnumerator =
        let inline current enumerator = (^a: (member Current: ^b) enumerator)
        let inline moveNext (enumerator: ^a byref) = (^a: (member MoveNext: unit -> bool) enumerator)
        let inline getEnumerator enumerator = (^a: (member GetEnumerator: unit -> SEnumerator<_,_>) enumerator)
        let inline getIEnumerator enumerator = (^a: (member GetEnumerator: unit -> IEnumerator<_>) enumerator)

    [<Struct; NoComparison; NoEquality>]
    type IEnumeratorWrapper< ^a> = {
        mutable Enumerator: IEnumerator< ^a>
    } with
        member inline this.MoveNext() = this.Enumerator.MoveNext()
        member inline this.Current = this.Enumerator.Current

    [<Struct; NoComparison; NoEquality>]
    type SWhereEnumerator2< ^a, ^b when ^a: struct
                                    and ^a: (member Current: ^b)
                                    and ^a: (member MoveNext: unit -> bool)> = {
        mutable Enumerator: SEnumerator< ^a,^b>
        Filter: ^b -> bool
    } with
        member inline this.MoveNext() =
            let mutable found = false
            while not found && SEnumerator.moveNext &this.Enumerator do
                found <- this.Enumerator |> SEnumerator.current |> this.Filter
            found
        member inline this.Current = this.Enumerator |> SEnumerator.current

        member inline this.Dispose() = ()

        member inline this.GetEnumerator() = this

    [<Struct; NoComparison; NoEquality>]
    type SWhereEnumerator< ^a, ^b when ^a: struct
                                   and ^a: (member Current: ^b)
                                   and ^a: (member MoveNext: unit -> bool)> =

        val mutable enumerator: SEnumerator< ^a,^b>
        val mutable filter: ^b -> bool

        member inline this.MoveNext() =
            let mutable found = false
            while not found && SEnumerator.moveNext &this.enumerator do
                found <- this.enumerator |> SEnumerator.current |> this.filter
            found

        member inline this.Current = this.enumerator |> SEnumerator.current

        member inline this.GetEnumerator() = this

        static member inline Create filter enumerator =
            let mutable enum = Unchecked.defaultof<SWhereEnumerator< ^a, ^b>>
            enum.enumerator <- enumerator
            enum.filter <- filter
            enum

    [<Struct; NoComparison; NoEquality>]
    type SSelectEnumerator2< ^a, ^b, ^c when ^a: struct
                                         and ^a: (member Current: ^b)
                                         and ^a: (member MoveNext: unit -> bool)> = {
        mutable Enumerator: SEnumerator< ^a, ^b>
        Map: ^b -> ^c
    } with
        member inline this.MoveNext() = SEnumerator.moveNext &this.Enumerator

        member inline this.Current = this.Enumerator |> SEnumerator.current |> this.Map

        member inline this.GetEnumerator() = this

    [<Struct; NoComparison; NoEquality>]
    type SIndexedEnumerator< ^a, ^b when ^a: struct
                                     and ^a: (member Current: ^b)
                                     and ^a: (member MoveNext: unit -> bool)> = {
        mutable Enumerator: SEnumerator< ^a, ^b>
        mutable Index: int
    } with
        member inline this.MoveNext() =
            this.Index <- this.Index + 1
            SEnumerator.moveNext &this.Enumerator
        member inline this.Current = struct (this.Index, this.Enumerator |> SEnumerator.current)
        member inline this.Dispose() = ()
        member inline this.GetEnumerator() = this

    [<Struct; NoComparison; NoEquality>]
    type SRefIndexedEnumerator< ^a, ^b when ^a: struct
                                        and ^a: (member Current: ^b)
                                        and ^a: (member MoveNext: unit -> bool)> = {
        mutable Enumerator: SEnumerator< ^a, ^b>
        mutable Index: int
    } with
        member inline this.MoveNext() =
            this.Index <- this.Index + 1
            SEnumerator.moveNext &this.Enumerator
        member inline this.Current = (this.Index, this.Enumerator |> SEnumerator.current)
        member inline this.Dispose() = ()
        member inline this.GetEnumerator() = this

    [<Struct>]
    type SArrayEnumerator< ^a> = {
        Array: ^a[]
        mutable Index: int
    } with
        member inline this.MoveNext() =
            this.Index <- this.Index + 1
            uint32 this.Index < uint32 this.Array.Length
        member inline this.Current = this.Array.[this.Index]
        member inline this.GetEnumerator() = this

    [<Struct; NoComparison; NoEquality>]
    type SSelectEnumerator< ^a, ^b, ^c when ^a: struct
                                        and ^a: (member Current: ^b)
                                        and ^a: (member MoveNext: unit -> bool)> =

        val mutable enumerator: SEnumerator< ^a, ^b>
        val mutable map: ^b -> ^c

        member inline this.MoveNext() = SEnumerator.moveNext &this.enumerator

        member inline this.Current = this.enumerator |> SEnumerator.current |> this.map

        member inline this.GetEnumerator() = this

        static member inline Create map enumerator =
            let mutable enum = Unchecked.defaultof<SSelectEnumerator< ^a, ^b, ^c>>
            enum.enumerator <- enumerator
            enum.map <- map
            enum

module ISeq =
    open InlineShapeEnumeratorsImpl
    let inline iter f seq =
        let mutable enum = seq
        while SEnumerator.moveNext &enum do
            SEnumerator.current enum |> f

    let inline ofArray array = { Array = array; Index = -1 }

    let inline filter filter seq = SWhereEnumerator.Create filter ^ SEnumerator.getEnumerator seq

    let inline filter2 filter seq =
        { Enumerator = SEnumerator.getEnumerator seq; Filter = filter }

    let inline indexed seq : SIndexedEnumerator<_,_> =
        { Enumerator = SEnumerator.getEnumerator seq; Index = -1 }

    let inline refIndexed seq : SRefIndexedEnumerator<_,_> =
        { Enumerator = SEnumerator.getEnumerator seq; Index = -1 }

    let inline map map seq = SSelectEnumerator.Create map ^ SEnumerator.getEnumerator seq

    let inline private minByImpl map (enum: 'a byref) =
        let mutable result = SEnumerator.current enum
        let mutable mapping = map result
        while SEnumerator.moveNext &enum do
            let value = SEnumerator.current enum
            let comparable = map value
            if mapping > comparable then
                result <- value
                mapping <- comparable
        result

    let inline minBy map seq =
        let mutable enum = seq
        if SEnumerator.moveNext &enum then
            minByImpl map &enum
        else
            invalidOp "Seq is empty"

    let inline tryMinBy map seq =
        let mutable enum = seq
        if SEnumerator.moveNext &enum then
            minByImpl map &enum
            |> Some
        else
            None

    let inline toResizeArray seq =
        let mutable enum = seq
        let rsz = ResizeArray()
        while SEnumerator.moveNext &enum do
            SEnumerator.current enum |> rsz.Add
        rsz

    let inline map2 map seq =
        { Enumerator = SEnumerator.getEnumerator seq; Map = map }