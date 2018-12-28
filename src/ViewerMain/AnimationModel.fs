namespace Animation

open Aardvark.Base.Incremental
open Aardvark.UI.Animation

open Provenance.Reduced

[<DomainType>]
type Animation = {
    model : AnimationModel
    target : CameraView option
    duration : RelativeTime
}

type AnimationAction =
    | AnimationAction of Aardvark.UI.Animation.AnimationAction
    | SetDuration     of RelativeTime
    | Animate         of CameraView
    | SetCamera       of CameraView
    | Stop

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Animation =

    let isAnimating (animation : Animation) =
        AnimationApp.shouldAnimate animation.model

    let duration (animation : Animation) =
        animation.duration

    let camera (animation : Animation) =
        CameraView.create animation.model.cam