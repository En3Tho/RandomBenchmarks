namespace Lib.FSharp.ValueOptionEnumerators

open System.Collections.Generic
open Lib.FSharp.Core

module ValueOptionEnumeratorsImlp =
    type VOEnumerator< ^a, ^b when ^a: struct                          
                               and ^a: (member MoveNext: unit -> ValueOption< ^b>)> = ^a

    module VOEnumerator =
        let inline moveNext (enumerator: ^a byref) = (^a: (member MoveNext: unit -> ValueOption< ^b>) enumerator)
        let inline getEnumerator enumerator = (^a: (member GetEnumerator: unit -> VOEnumerator<_,_>) enumerator)
        let inline getIEnumerator enumerator = (^a: (member GetEnumerator: unit -> IEnumerator<_>) enumerator)

    [<Struct; NoComparison; NoEquality>]
    type VOWhereEnumerator2< ^a, ^b when ^a: struct
                                    and ^a: (member MoveNext: unit -> ValueOption< ^b>)> = {
        mutable Enumerator: VOEnumerator< ^a,^b>
        Filter: ^b -> bool
    } with
        member inline this.MoveNext() =
            let mutable result = ValueNone
            while
                match VOEnumerator.moveNext &this.Enumerator with
                | ValueSome value when this.Filter value ->
                    result <- value |> ValueSome
                    false
                | ValueSome _ ->
                    true
                | _ ->
                    false
                do()
            result

        member inline this.GetEnumerator() = this

    [<Struct; NoComparison; NoEquality>]
    type VOWhereEnumerator< ^a, ^b when ^a: struct
                                   and ^a: (member MoveNext: unit -> ValueOption< ^b>)> =

        val mutable enumerator: VOEnumerator< ^a,^b>
        val mutable filter: ^b -> bool

        member inline this.MoveNext() =
            let mutable result = ValueNone
            while
                match VOEnumerator.moveNext &this.enumerator with
                | ValueSome value when this.filter value ->
                    result <- value |> ValueSome
                    false
                | ValueSome _ ->
                    true
                | _ ->
                    false
                do()
            result

        member inline this.GetEnumerator() = this

        static member inline Create filter enumerator =
            let mutable enum = Unchecked.defaultof<VOWhereEnumerator< ^a, ^b>>
            enum.enumerator <- enumerator
            enum.filter <- filter
            enum

    [<Struct; NoComparison; NoEquality>]
    type VOSelectEnumerator2< ^a, ^b, ^c when ^a: struct
                                         and ^a: (member MoveNext: unit -> ValueOption< ^b>)> = {
        mutable Enumerator: VOEnumerator< ^a, ^b>
        Map: ^b -> ^c
    } with
        member inline this.MoveNext() =
            match VOEnumerator.moveNext &this.Enumerator with
            | ValueSome value -> this.Map value |> ValueSome
            | _ -> ValueNone

        member inline this.GetEnumerator() = this

    [<Struct; NoComparison; NoEquality>]
    type VOIndexedEnumerator< ^a, ^b when ^a: struct
                                     and ^a: (member MoveNext: unit -> ValueOption< ^b>)> = {
        mutable Enumerator: VOEnumerator< ^a, ^b>
        mutable Index: int
    } with
        member inline this.MoveNext() =
            this.Index <- this.Index + 1
            match VOEnumerator.moveNext &this.Enumerator with
            | ValueSome value -> struct (this.Index, value) |> ValueSome
            | _ -> ValueNone

        member inline this.GetEnumerator() = this

    [<Struct; NoComparison; NoEquality>]
    type VORefIndexedEnumerator< ^a, ^b when ^a: struct
                                        and ^a: (member MoveNext: unit -> ValueOption< ^b>)> = {
        mutable Enumerator: VOEnumerator< ^a, ^b>
        mutable Index: int
    } with
        member inline this.MoveNext() =
            this.Index <- this.Index + 1
            match VOEnumerator.moveNext &this.Enumerator with
            | ValueSome value -> (this.Index, value) |> ValueSome
            | _ -> ValueNone

        member inline this.GetEnumerator() = this

    [<Struct>]
    type VOArrayEnumerator< ^a> = {
        Array: ^a[]
        mutable Index: int
    } with
        member inline this.MoveNext() =
            this.Index <- this.Index + 1
            if uint32 this.Index < uint32 this.Array.Length then
                ValueSome this.Array.[this.Index]
            else
                ValueNone

        member inline this.GetEnumerator() = this

    [<Struct; NoComparison; NoEquality>]
    type VOSelectEnumerator< ^a, ^b, ^c when ^a: struct
                                        and ^a: (member MoveNext: unit -> ValueOption< ^b>)> =

        val mutable enumerator: VOEnumerator< ^a, ^b>
        val mutable map: ^b -> ^c

        member inline this.MoveNext() =
            match VOEnumerator.moveNext &this.enumerator with
            | ValueSome value -> this.map value |> ValueSome
            | _ -> ValueNone

        member inline this.GetEnumerator() = this

        static member inline Create map enumerator =
            let mutable enum = Unchecked.defaultof<VOSelectEnumerator< ^a, ^b, ^c>>
            enum.enumerator <- enumerator
            enum.map <- map
            enum

module VOSeq =
    open ValueOptionEnumeratorsImlp
    let inline iter f seq =
        let mutable enum = seq
        while
            match VOEnumerator.moveNext &enum with
            | ValueSome value ->
                value |> f
                true
            | _ -> false
            do ()

    let inline ofArray array = { Array = array; Index = -1 }

    let inline filter filter seq = VOWhereEnumerator.Create filter ^ VOEnumerator.getEnumerator seq

    let inline filter2 filter seq =
        { Enumerator = VOEnumerator.getEnumerator seq; Filter = filter }

    let inline indexed seq : VOIndexedEnumerator<_,_> =
        { Enumerator = VOEnumerator.getEnumerator seq; Index = -1 }

    let inline refIndexed seq : VORefIndexedEnumerator<_,_> =
        { Enumerator = VOEnumerator.getEnumerator seq; Index = -1 }

    let inline map map seq = VOSelectEnumerator.Create map ^ VOEnumerator.getEnumerator seq

    let inline private minByImpl map (enum: 'a byref) (initial: 'b) =
        let mutable result = initial
        let mutable mapping = map result
        while
            match VOEnumerator.moveNext &enum with
            | ValueSome value ->
                let comparable = map value
                if mapping > comparable then
                    result <- value
                    mapping <- comparable
                true
            | _ -> false
            do ()
        result

    let inline minBy map seq =
        let mutable enum = seq
        match VOEnumerator.moveNext &enum with
        | ValueSome value -> minByImpl map &enum value
        | _ -> invalidOp "Seq is empty"

    let inline tryMinBy map seq =
        let mutable enum = seq
        match VOEnumerator.moveNext &enum with
        | ValueSome value -> minByImpl map &enum value |> ValueSome
        | _ -> ValueNone

    let inline toResizeArray seq =
        let mutable enum = seq
        let rsz = ResizeArray()
        while
            match VOEnumerator.moveNext &enum with
            | ValueSome value ->
                value |> rsz.Add
                true
            | _ -> false
            do ()
        rsz

    let inline map2 map seq =
        { Enumerator = VOEnumerator.getEnumerator seq; Map = map }