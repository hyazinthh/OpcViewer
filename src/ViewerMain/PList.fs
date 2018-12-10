namespace Aardvark.Base

open System
open System.Collections.Generic

[<AutoOpen>]
module PListExtensions =
    module PList =
        let isEmpty (list : 'a plist) =
            list.Count = 0
       
        let tryFindIndex (predicate : 'a -> bool) (list : 'a plist) =
            predicate |> list.FirstIndexOf |> list.TryGetIndex

        let findIndex (predicate : 'a -> bool) (list : 'a plist) =
            match (list |> tryFindIndex predicate) with
                | None -> raise (KeyNotFoundException ())
                | Some i -> i

        let tryFindIndex' (predicate : 'a -> bool) (list : 'a plist) =
            match list.FirstIndexOf predicate with
                | -1 -> None
                | i -> Some i

        let findIndex' (predicate : 'a -> bool) (list : 'a plist) =
            match (list |> tryFindIndex' predicate) with
                | None -> raise (KeyNotFoundException ())
                | Some i -> i

        let tryFind (predicate : 'a -> bool) (list : 'a plist) =
            list |> tryFindIndex predicate |> Option.bind list.TryGet

        let find (predicate : 'a -> bool) (list : 'a plist) =
            match (list |> tryFind predicate) with
                | None -> raise (KeyNotFoundException ())
                | Some v -> v

        let tryHead (list : 'a plist) =
            list.TryGet list.MinIndex

        let head (list : 'a plist) =
            match list |> tryHead with
                | None -> raise (ArgumentException ("List is empty"))
                | Some v -> v

        let tryLast (list : 'a plist) =
            list.TryGet list.MaxIndex

        let last (list : 'a plist) =
            match list |> tryLast with
                | None -> raise (ArgumentException ("List is empty"))
                | Some v -> v

        let right (index : Index) (list : 'a plist) =
            let (_, _, r) = list |> PList.toMap |> MapExt.neighbours index
            r |> Option.map snd

        let left (index : Index) (list : 'a plist) =
            let (l, _, _) = list |> PList.toMap |> MapExt.neighbours index
            l |> Option.map snd

        let get (index : Index) (list : 'a plist) =
            match PList.tryGet index list with
                | None -> raise (KeyNotFoundException ())
                | Some x -> x