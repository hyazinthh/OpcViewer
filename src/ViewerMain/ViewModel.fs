namespace View

open Aardvark.Base.Incremental

open Provenance.Reduced
open Animation
open Model

[<DomainType>]
type PresentationParams = {
    rendering : RenderingParams
}

[<DomainType>]
type ViewParams = {
    camera : CameraView
    presentation : PresentationParams
}

[<DomainType>]
type View = {
    state : ViewParams
    preview : ViewParams option
    animation : Animation
}

type ViewAction =
    | Set
    | Move
    | Preview
    | StopPreview
    | Animation     of AnimationAction

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module View =

    let camera (view : View) =
        Animation.camera view.animation

    let viewParams (view : View) =
        match view.preview with
            | None -> view.state
            | Some x -> x