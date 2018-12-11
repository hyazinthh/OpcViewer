namespace Model

open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.Application
open Aardvark.UI.Primitives
open Aardvark.UI.Animation

open OpcSelectionViewer
open Provenance
open Story
open Animation
open Session

type Action =
    | AppAction             of AppAction
    | ProvenanceAction      of ProvenanceAction
    | StoryAction           of StoryAction
    | SessionAction         of SessionAction
    | UpdateConfig          of DockConfig
    | NodeClick             of NodeId
    | AnimationAction       of AnimationAction
    | KeyDown               of Keys
    | KeyUp                 of Keys
    | RenderControlResized  of V2i

[<DomainType>]
type Model = {
    appModel : AppModel
    dockConfig : DockConfig
    provenance : Provenance
    story : Story
    animation : Animation
    renderControlSize : V2i
    directory : string
}

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module AppModel =

    let setView (view : CameraView) (model : AppModel) =
        { model with camera = { model.camera with view = view } }

    let getView (model : AppModel) =
        model.camera.view |> Reduced.CameraView.create

    let setRendering (rendering  : RenderingParams) (model : AppModel) =
        { model with fillMode = rendering.fillMode }

    let getRendering (model : AppModel) =
        { fillMode = model.fillMode }

    let setPresentation (presentation : PresentationParams) (model : AppModel) =
        model |> setView (presentation.view |> Reduced.CameraView.restore)
              |> setRendering presentation.rendering

    let getPresentation (model : AppModel) =
        { view = model |> getView;
          rendering = model |> getRendering }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Model =

    let setView (view : CameraView) (model : Model) =
        { model with appModel = model.appModel |> AppModel.setView view }

    let getView (model : Model) =
        model.appModel.camera.view

    let setRendering (rendering  : RenderingParams) (model : Model) =
        { model with appModel = model.appModel |> AppModel.setRendering rendering }

    let getRendering (model : Model) =
        model.appModel |> AppModel.getRendering

    let setPresentation (presentation : PresentationParams) (model : Model) =
        { model with appModel = model.appModel |> AppModel.setPresentation presentation }

    let getPresentation (model : Model) =
        model.appModel |> AppModel.getPresentation

    let isAnimating (model : Model) =
        model.animation.model |> AnimationApp.shouldAnimate
