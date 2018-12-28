namespace Story

open System
open System.Collections.Generic
open Aardvark.Base
open Aardvark.Base.Incremental

open Model
open Provenance
open Annotations
open Thumbnail
open View

// A slide can either contain raw text
// or a frame referencing an analysis state from the provenance graph
[<DomainType>]
type Content =
    | TextContent
    | FrameContent of Node * ViewParams * Annotations

    static member isText = function
        | TextContent _ -> true
        | _ -> false

    static member isFrame = function
        | FrameContent _ -> true
        | _ -> false

// Each slide has an id
type SlideId = 
    private SlideId of Guid with

    static member generate () =
        SlideId (Guid.NewGuid ())

    static member ofGuid (v : Guid) =
        SlideId v

    static member parse (s : string) =
        s |> Guid.Parse |> SlideId

    static member tryParse (s : string) =
        try
            Some (s |> Guid.Parse |> SlideId)
        with
            | _ -> None

    override x.ToString () =
        let (SlideId v) = x in string v

// The slide record
[<DomainType>]
type Slide = {
    [<PrimaryKey>]
    id : SlideId
    content : Content 
    thumbnail : Thumbnail
}

[<DomainType>]
type Selection = {
    current : Slide
    modified : Slide
}

 // A story is a list of slides and optionally
 // a currently selected slide with unsaved changes
[<DomainType>]
type Story = {
    slides : Slide plist
    selected : Selection option
    showAnnotations : bool
    thumbnailRequests : SlideId hset
    presentation : bool
}

type StoryAction =
    | UpdateFrame
    | AnnotationAction   of AnnotationAction
    | ThumbnailAction    of SlideId * ThumbnailAction
    | Forward
    | Backward
    | Commit
    | StartPresentation
    | EndPresentation
    | SelectSlide        of SlideId
    | RemoveSlide        of SlideId
    | MoveSlide          of SlideId * SlideId option * SlideId option
    | AddFrameSlide      of SlideId option
    | AddTextSlide       of SlideId option
    | DuplicateSlide     of SlideId
    | DeselectSlide
    | MouseEnterSlide    of SlideId
    | MouseLeaveSlide
    | ToggleAnnotations

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Slide =

    let id (slide : Slide) = slide.id

    let content (slide : Slide) = slide.content

    let isText (slide : Slide) =
        slide |> content |> Content.isText

    let isFrame (slide : Slide) =
        slide |> content |> Content.isFrame

    let duplicate (slide : Slide) =
        { slide with id = SlideId.generate () }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Selection =

    let current (s : Selection) = s.current

    let modified (s : Selection) = s.modified

    let select (s : Slide) =
        { current = s; modified = s }

    let commit (s : Selection) =
        { s with current = s.modified }

    let reset (s : Selection) =
        { s with modified = s.current }

    let map (f : Slide -> Slide) (s : Selection) =
        { s with current = f s.current; modified = f s.modified }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Story =

    [<AutoOpen>]
    module private Helpers =

        let tryFindInListById (id : SlideId) (s : Story) =
            s.slides |> PList.tryFindIndex (fun s -> s.id = id)

        let findInListById (id : SlideId) (s : Story) =
            s.slides |> PList.findIndex (fun s -> s.id = id)

        let findInList (slide : Slide) (s : Story) =
            s |> findInListById slide.id

    let isActive (s : Story) =
        Option.isSome s.selected

    let trySelected (s : Story) = s.selected

    let selected (s : Story) =
        match trySelected s with
            | None -> raise (ArgumentException ("No slide selected"))
            | Some s -> s

    let isSelected (slide : Slide) (s : Story) =
        s.selected |> Option.map (fun s -> s.current.id = slide.id)
                   |> Option.defaultValue false

    let tryFindIndex (slide : Slide) (s : Story) =
        s.slides |> PList.tryFindIndex' (fun s -> s.id = slide.id)

    let findIndex (slide : Slide) (s : Story) =
        match (s |> tryFindIndex slide) with
            | None -> raise (KeyNotFoundException ())
            | Some i -> i

    let tryFind (predicate : Slide -> bool) (s : Story) =
        s.slides |> PList.tryFind predicate

    let find (predicate : Slide -> bool) (s : Story) =
        match (s |> tryFind predicate) with
            | None -> raise (KeyNotFoundException ())
            | Some s -> s

    let tryFindById (id : SlideId) (s : Story) =
        s |> tryFind (fun s -> s.id = id)

    let findById (id : SlideId) (s : Story) =
        s |> find (fun s -> s.id = id)

    let contains (slide : Slide) (s : Story) =
        s |> tryFindById slide.id |> Option.isSome

    let rightOf (slide : Slide) (s : Story) =
        s.slides |> PList.right (s |> findInList slide)

    let leftOf (slide : Slide) (s : Story) =
        s.slides |> PList.left (s |> findInList slide)

    let first (s : Story) =
        s.slides |> PList.tryHead

    let last (s : Story) =
        s.slides |> PList.tryLast

    let select (slide : Slide option) (s : Story) =
        match slide with
            | None -> { s with selected = None }
            | Some x -> { s with selected = Some <| Selection.select x }

    let selectById (id : SlideId option) (s : Story) =
        s |> select (id |> Option.map (fun id -> s |> findById id))

    let startPresentation (s : Story) =
        match s.selected with
            | None -> s |> select (first s)
            | _ -> s
        |> fun s -> { s with presentation = true }

    let endPresentation (s : Story) =
        { s with presentation = false }

    let forward (s : Story) =
        let r = s.selected |> Option.bind (fun x ->
                    s |> rightOf x.current
                )

        if r.IsNone then s else s |> select r

    let backward (s : Story) =
        let l = s.selected |> Option.bind (fun x ->
                    s |> leftOf x.current
                )

        if l.IsNone then s else s |> select l

    let append (slide : Slide) (s : Story) =
        { s with slides = s.slides |> PList.append slide }
            |> select (Some slide)

    let insertBefore (slide : Slide) (before : Slide) (s : Story) =
        { s with slides = s.slides |> PList.insertBefore (s |> findInList before) slide }
            |> select (Some slide)

    let insertBeforeById (slide : Slide) (before : SlideId) (s : Story) =
        s |> insertBefore slide (s |> findById before)

    let insertAfter (slide : Slide) (after : Slide) (s : Story) =
        { s with slides = s.slides |> PList.insertAfter (s |> findInList after) slide }
            |> select (Some slide)

    let insertAfterById (slide : Slide) (after : SlideId) (s : Story) =
        s |> insertAfter slide (s |> findById after)

    let remove (slide : Slide) (s : Story) =
        { s with slides = s.slides |> PList.remove (s |> findInList slide)
                 selected = if isSelected slide s then None else s.selected }

    let removeById (id : SlideId) (s : Story) =
        s |> remove (s |> findById id)

    let duplicate (slide : Slide) (s : Story) =
        let d = Slide.duplicate slide
        s |> insertAfter d slide

    let duplicateById (id : SlideId) (s : Story) =
        s |> duplicate (s |> findById id)

    let tryUpdateById (id : SlideId) (f : Slide -> Slide) (s : Story) =
        match s |> tryFindInListById id with
            | None -> s
            | Some idx ->
                { s with slides = s.slides |> PList.update idx f 
                         selected = s.selected |> Option.filter (fun s -> s.current.id = id)
                                               |> Option.map (Selection.map f) }

    let moveBefore (slide : Slide) (before : Slide) (s : Story) =
        { s with slides = s.slides |> PList.remove (s |> findInList slide)
                                   |> PList.insertBefore (s |> findInList before) slide }

    let moveBeforeById (slide : SlideId) (before : SlideId) (s : Story) =
        s |> moveBefore (s |> findById slide) (s |> findById before)

    let moveAfter (slide : Slide) (after : Slide) (s : Story) =
        { s with slides = s.slides |> PList.remove (s |> findInList slide)
                                   |> PList.insertAfter (s |> findInList after) slide }

    let moveAfterById (slide : SlideId) (after : SlideId) (s : Story) =
        s |> moveAfter (s |> findById slide) (s |> findById after)

    let length (s : Story) =
        s.slides |> PList.count

    // Updates the story with changes made to the selected slide
    let commit (s : Story) =
        match s.selected with
            | None -> s 
            | Some sel -> { s with slides = s.slides |> PList.set (s |> findInList sel.modified) sel.modified
                                   selected = Some <| Selection.commit sel }

    // Resets changes made to the selected slide
    let reset (s : Story) =
        match s.selected with
            | None -> s
            | Some sel -> { s with selected = Some <| Selection.reset sel }

    let isNodeReferenced (node : Node) (ignoreSelected : bool) (s : Story) =
        s.slides |> PList.tryFind (fun x ->
            match x.content with
                | TextContent _ -> false
                | FrameContent (n, _, _) -> (n.id = node.id) && not (ignoreSelected && isSelected x s)
        ) |> Option.isSome
