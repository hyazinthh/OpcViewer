namespace Model

open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.Application
open Aardvark.UI.Primitives

open OpcSelectionViewer
open Provenance
open Provenance.Reduced
open Story
open Session
open View
open Animation

type Action =
    | AppAction             of AppAction
    | ProvenanceAction      of ProvenanceAction
    | StoryAction           of StoryAction
    | SessionAction         of SessionAction
    | UpdateConfig          of DockConfig
    | NodeClick             of NodeId
    | ViewAction            of ViewAction
    | KeyDown               of Keys
    | KeyUp                 of Keys
    | RenderControlResized  of V2i

[<DomainType>]
type Model = {
    appModel : AppModel
    view : View
    dockConfig : DockConfig
    provenance : Provenance
    story : Story
    renderControlSize : V2i
    directory : string
}

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module AppModel =

    let setCamera (camera : CameraView) (model : AppModel) =
        { model with camera = { model.camera with view = CameraView.restore camera } }

    let getCamera (model : AppModel) =
        model.camera.view |> CameraView.create

    let setRendering (rendering  : RenderingParams) (model : AppModel) =
        { model with fillMode = rendering.fillMode }

    let getRendering (model : AppModel) =
        { fillMode = model.fillMode }

    let setPresentation (presentation : PresentationParams) (model : AppModel) =
        model |> setRendering presentation.rendering

    let getPresentation (model : AppModel) =
        { rendering = getRendering model }

    let setViewParams (view : ViewParams) (model : AppModel) =
        model |> setCamera view.camera
              |> setPresentation view.presentation

    let getViewParams (model : AppModel) =
        { camera = model |> getCamera
          presentation = model |> getPresentation }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Model =

    let setCamera (camera : CameraView) (model : Model) =
        { model with appModel = model.appModel |> AppModel.setCamera camera }

    let getCamera (model : Model) =
        model.appModel |> AppModel.getCamera

    let setRendering (rendering  : RenderingParams) (model : Model) =
        { model with appModel = model.appModel |> AppModel.setRendering rendering }

    let getRendering (model : Model) =
        model.appModel |> AppModel.getRendering

    let setPresentation (presentation : PresentationParams) (model : Model) =
        { model with appModel = model.appModel |> AppModel.setPresentation presentation }

    let getPresentation (model : Model) =
        model.appModel |> AppModel.getPresentation

    let setViewParams (view : ViewParams) (model : Model) =
        { model with appModel = model.appModel |> AppModel.setViewParams view }

    let getViewParams (model : Model) =
        model.appModel |> AppModel.getViewParams

    let isAnimating (model : Model) =
        model.view.animation |> Animation.isAnimating

    let isPreview (model : Model) =
        model.view.preview |> Option.isSome