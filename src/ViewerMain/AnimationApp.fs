module AnimationApp

open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.UI.Animation

open Animation
open Provenance.Reduced

[<AutoOpen>]
module Helpers =

    // Sets the camera
    let setCamera (camera : CameraView) (animation : Animation) =
        animation |> Lens.set (Animation.Lens.model |. AnimationModel.Lens.cam) (CameraView.restore camera)

    // Manually set the end camera position when the animation finished
    // This needs to be done since the underlying animation system is buggy and does not actually return the final state.
    let finalizeIfFinished (prev : Animation) (animation : Animation) =

        let finalize (animation : Animation) =
            match animation.target with
                | None -> animation
                | Some t -> animation |> setCamera t
                                      |> Lens.set Animation.Lens.target None

        if not (Animation.isAnimating animation) && Animation.isAnimating prev then
            finalize animation
        else
            animation

    // Stops and removes all animations
    let stop (animation : Animation) =
        animation |> Lens.set Animation.Lens.target None
                  |> Lens.set (Animation.Lens.model |. AnimationModel.Lens.animations) PList.empty

    // Creates an animation towards the target camera position
    let animate (target : CameraView) (animation : Animation) =

        if Animation.camera animation = target then
            stop animation
        else if Option.contains target animation.target then
            animation
        else
            let a = CameraAnimations.interpolate (CameraView.restore target) animation.duration ""

            animation |> stop
                      |> Lens.set Animation.Lens.target (Some target)
                      |> Lens.update Animation.Lens.model (fun m -> AnimationApp.update m <| PushAnimation a)

// Initialize animations
let init (camera : CameraView) =
    { model = { cam = CameraView.restore camera; animation = Animate.On; animations = PList.empty }
      target = None
      duration = 0.5 }

// Handles animation messages
let update (msg : AnimationAction) (animation : Animation) =
    match msg with
        | AnimationAction a ->
            animation |> Lens.update Animation.Lens.model (fun m -> AnimationApp.update m a)
                        |> finalizeIfFinished animation

        | SetDuration t ->
            { animation with duration = t }

        | Animate c ->
            animation |> animate c

        | SetCamera c ->
            animation |> stop
                      |> setCamera c

        | Stop ->
            stop animation

let threads (animation : Animation) =
    animation.model |> AnimationApp.ThreadPool.threads
                    |> ThreadPool.map AnimationAction