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
    animation : Animation
}

type ViewAction =
    | Set
    | Move
    | Preview
    | Animation     of AnimationAction

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module View =

    let camera (view : View) =
        Animation.camera view.animation