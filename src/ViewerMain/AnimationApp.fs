module AnimationApp

open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.UI.Animation

open Model
open Animation
open Provenance

[<AutoOpen>]
module Helpers =
    // Updates the animations
    let updateCamera (prev : Model) (model : Model) =

        if Model.isAnimating model then
            model |> Model.setView model.animation.model.cam
        else
            // TODO: We need to do this cause the animation system is buggy, see below
            if Model.isAnimating prev then
                model |> Model.setView model.animation.savedView
            else
                model

    // Sets the current view for the animations and saves the
    // current / destination view. We need to save the final view since
    // the animation system is buggy and does not actually return the final state.
    // Instead it is set manually once the animation is finished.
    let setView (view : CameraView) (model : Model) =
        model |> Lens.set (Model.Lens.animation |. Animation.Lens.model |. AnimationModel.Lens.cam) view
              |> Lens.set (Model.Lens.animation |. Animation.Lens.savedView) (Model.getView model)

    // Removes all animations
    let removeAll (model : Model) =
        let l = Model.Lens.animation |. Animation.Lens.model |. AnimationModel.Lens.animations
        model |> Lens.set l PList.empty

let init (model : AppModel) = 
    { model = { cam = model.camera.view; animation = Animate.On; animations = PList.empty }
      savedView = model.camera.view }

// Handles animation messages
let update (msg : AnimationAction) (model : Model) =
    model |> Lens.set (Model.Lens.animation |. Animation.Lens.model) (msg |> AnimationApp.update model.animation.model)
          |> updateCamera model

// Initializes a camera transition from src to dest
let animate (duration : RelativeTime) (src : Model) (dest : Model) =
    let srcView = src |> Model.getView
    let destView = dest |> Model.getView

    if Reduced.CameraView.equal srcView destView then
        dest
    else
        let animation = CameraAnimations.interpolate destView duration ""

        dest |> setView srcView
             |> removeAll
             |> update (PushAnimation animation)

let threads (model : Model) =
    model.animation.model |> AnimationApp.ThreadPool.threads
                          |> ThreadPool.map AnimationAction