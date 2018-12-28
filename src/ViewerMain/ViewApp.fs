module ViewApp

open Aardvark.Base
open Aardvark.Base.Incremental

open Provenance.Reduced

open View
open Model
open Animation

let init (view : ViewParams) =
    { state = view
      preview = None
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
                view |> Lens.update View.Lens.animation (moveCamera true v.camera)
                     |> Lens.set View.Lens.state v

            | Preview ->
                view |> Lens.update View.Lens.animation (moveCamera true v.camera)
                     |> Lens.set View.Lens.preview (Some v)

            | StopPreview ->
                view |> Lens.update View.Lens.animation (moveCamera true view.state.camera)
                     |> Lens.set View.Lens.preview None

    let getViewParams (view : View) =
        { camera = Animation.camera view.animation
          presentation = view.preview |> Option.map (fun x -> x.presentation)
                                      |> Option.defaultValue view.state.presentation }

    let v = processView model.view

    model |> Lens.set (Model.Lens.view) v
          |> Model.setViewParams (getViewParams v)

let threads (model : Model) =
    model.view.animation |> AnimationApp.threads
                         |> ThreadPool.map Animation
