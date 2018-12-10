namespace Annotations

open System

open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.UI

// Each annotation has an id
type AnnotationId = 
    private AnnotationId of Guid with

    static member generate () =
        AnnotationId (Guid.NewGuid ())

    static member ofGuid (v : Guid) =
        AnnotationId v

    static member parse (s : string) =
        s |> Guid.Parse |> AnnotationId

    static member tryParse (s : string) =
        try
            Some (s |> Guid.Parse |> AnnotationId)
        with
            | _ -> None

    override x.ToString () =
        let (AnnotationId v) = x in string v

// A label displays text at a given position
[<DomainType>]
type Label = {
    width : int
    position : V2i
    text : string
}

// An annotation is a label and optionally a target
// in world space that is to be annotated
[<DomainType>]
type Annotation = {
    id : AnnotationId
    target : V3d option
    label : Label
}

[<DomainType>]
type Annotations = {
    list : Annotation plist
    focus : Annotation option
    targeting : bool
}

type AnnotationAction =
    | LabelChanged  of AnnotationId * string
    | LabelMoved    of AnnotationId * V2i
    | LabelResized  of AnnotationId * int
    | LabelClicked  of AnnotationId
    | Focus         of AnnotationId
    | Blur
    | Add
    | Remove        of AnnotationId
    | Target
    | SaveTarget    of V3d

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Label =
    let create (position : V2i) = {
        width = 300;
        position = position
        text = String.Empty
    }

[<AutoOpen>]
 module Events =
    // Fired after a label is resized
    let onLabelResized (cb : int -> 'msg) =
        onEvent "onlabelresized" [] (List.head >> Pickler.unpickleOfJson >> cb)

    // Fired after a label is moved
    let onLabelMoved (cb : V2i -> 'msg) =
        onEvent "onlabelmoved" [] (List.head >> Pickler.unpickleOfJson >> cb)

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Annotation =
    let create (target : V3d option) (label : Label) = {
        id = AnnotationId.generate ()
        target = target
        label = label
    }

    let setLabelText (text : string) (a : Annotation) =
        { a with label = { a.label with text = text }}

    let setLabelPosition (pos : V2i) (a : Annotation) =
        { a with label = { a.label with position = pos }}

    let setLabelWidth (width : int) (a : Annotation) =
        { a with label = { a.label with width = width }}

    let setTarget (target : V3d option) (a : Annotation) =
        { a with target = target }