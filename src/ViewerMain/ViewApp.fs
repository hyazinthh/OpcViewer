module ViewApp

open Aardvark.Base
open Aardvark.Base.Incremental

open Provenance.Reduced

open View
open Model
open Preview
open Animation

let init (view : ViewParams) =
    { state = view
      animation = AnimationApp.init view.camera }

let update (msg : ViewAction) (model : Model) =

    let processView (view : View) =
        let moveCamera (animate : bool) (camera : CameraView) =
            AnimationApp.update <| if animate then Animate camera else SetCamera camera

        let v = Model.getViewParams model

        match msg with
            | Animation a ->
                view |> Lens.update View.Lens.animation (AnimationApp.update a)

            | Set ->
                view |> Lens.update View.Lens.animation (moveCamera false v.camera)
                     |> Lens.set View.Lens.state v

            | Move ->
                view |> if Model.isPreviewMode View model then      // If there is a view preview going on, don't animate
                            id
                        else 
                            Lens.update View.Lens.animation (moveCamera true v.camera)
                     |> Lens.set View.Lens.state v

            | Preview ->
                let v = Model.getActiveViewParams model
                view |> Lens.update View.Lens.animation (moveCamera true v.camera)

    let getViewParams (view : View) =
        { camera = Animation.camera view.animation
          presentation = Model.getActivePresentation model }

    let v = processView model.view

    model |> Lens.set (Model.Lens.view) v
          |> Model.setActiveViewParams (getViewParams v)

let threads (model : Model) =
    model.view.animation |> AnimationApp.threads
                         |> ThreadPool.map Animation
