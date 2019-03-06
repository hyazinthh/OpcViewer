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
open Preview
open Animation

type Action =
    | AppAction             of AppAction
    | ProvenanceAction      of ProvenanceAction
    | StoryAction           of StoryAction
    | SessionAction         of SessionAction
    | UpdateConfig          of DockConfig
    | NodeClick             of NodeId
    | ViewAction            of ViewAction
    | PreviewAction         of PreviewAction
    | KeyDown               of Keys
    | KeyUp                 of Keys
    | RenderControlResized  of V2i

[<DomainType>]
type InnerModel = {
    current : AppModel          // The current state of the inner application
    preview : Preview option    // A preview or temporary state
    output : AppModel           // The state that is displayed (may be a preview)
}

[<DomainType>]
type Model = {
    inner : InnerModel
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
module InnerModel =

    // Gets the active encapsulated model according to the given mode.
    // E.g. when a preview of a camera position is ongoing, the preview model is returned
    // when calling this function for mode = View
    let getActiveModel (mode : Mode) (model : InnerModel) =
        model.preview |> Option.filter (Preview.is mode)
                      |> Option.map (fun p -> p.model)
                      |> Option.defaultValue model.current

    let setCamera (camera : CameraView) (model : InnerModel) =
        { model with current = model.current |> AppModel.setCamera camera }

    let getCamera (model : InnerModel) =
        model.current |> AppModel.getCamera

    let setRendering (rendering  : RenderingParams) (model : InnerModel) =
        { model with current = model.current |> AppModel.setRendering rendering }

    let getRendering (model : InnerModel) =
        model.current |> AppModel.getRendering

    let setPresentation (presentation : PresentationParams) (model : InnerModel) =
        { model with current = model.current |> AppModel.setPresentation presentation }

    let getPresentation (model : InnerModel) =
        model.current |> AppModel.getPresentation

    let setViewParams (view : ViewParams) (model : InnerModel) =
        { model with current = model.current |> AppModel.setViewParams view }

    let setActiveViewParams (view : ViewParams) (model : InnerModel) =
        match model.preview with
            | Some p when p |> Preview.is View ->
                { model with preview = Some { p with model = p.model |> AppModel.setViewParams view } }
            | _ ->
                { model with current = model.current |> AppModel.setViewParams view }

    let getViewParams (model : InnerModel) =
        model.current |> AppModel.getViewParams

    let getActiveViewParams (model : InnerModel) =
        model.preview |> Option.filter (Preview.is View)
                      |> Option.map (fun p -> p.model)
                      |> Option.defaultValue model.current
                      |> AppModel.getViewParams

    let getActivePresentation (model : InnerModel) =
        model |> getActiveViewParams
              |> fun v -> v.presentation

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Model =

    let getActiveModel (mode : Mode) (model : Model) =
        model.inner |> InnerModel.getActiveModel mode

    let setCamera (camera : CameraView) (model : Model) =
        { model with inner = model.inner |> InnerModel.setCamera camera }

    let getCamera (model : Model) =
        model.inner |> InnerModel.getCamera

    let setRendering (rendering  : RenderingParams) (model : Model) =
        { model with inner = model.inner |> InnerModel.setRendering rendering }

    let getRendering (model : Model) =
        model.inner |> InnerModel.getRendering

    let setPresentation (presentation : PresentationParams) (model : Model) =
        { model with inner = model.inner |> InnerModel.setPresentation presentation }

    let getPresentation (model : Model) =
        model.inner |> InnerModel.getPresentation

    let getActivePresentation (model : Model) =
        model.inner |> InnerModel.getActivePresentation

    let setViewParams (view : ViewParams) (model : Model) =
        { model with inner = model.inner |> InnerModel.setViewParams view }

    let setActiveViewParams (view : ViewParams) (model : Model) =
        { model with inner = model.inner |> InnerModel.setActiveViewParams view }

    let getViewParams (model : Model) =
        model.inner |> InnerModel.getViewParams

    let getActiveViewParams (model : Model) =
        model.inner |> InnerModel.getActiveViewParams

    let isAnimating (model : Model) =
        model.view.animation |> Animation.isAnimating

    let isPreview (model : Model) =
        model.inner.preview |> Option.isSome

    let isPreviewMode (mode : Mode) (model : Model) =
        model.inner.preview |> Option.exists (Preview.is mode)