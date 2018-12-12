module App

open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.UI
open Aardvark.UI.Primitives
open Aardvark.Application

open Model
open Provenance
open Story

[<AutoOpen>]
module private Events =
    let onResize (cb : V2i -> 'msg) =
        onEvent "onresize" ["{ X: $(document).width(), Y: $(document).height() }"] (List.head >> Pickler.json.UnPickleOfString >> cb)

let regularModeConfig =
    config {
        content (
            vertical 1.0 [
                horizontal 5.0 [
                    element { id "render"; title "Render View"; isCloseable false; weight 10 }

                    stack 3.5 (Some "controls") [
                        { id = "controls"; title = Some "Controls"; weight = 1.0; deleteInvisible = None; isCloseable = Some true }
                        { id = "presentation"; title = Some "Presentation"; weight = 1.0; deleteInvisible = None; isCloseable = Some true }
                    ]
                ]

                element { id "provenance"; title "History"; isCloseable true; weight 1 }
                element { id "storyboard"; title "Storyboard"; isCloseable true; weight 1.25 }
            ]
        )
        appName "Box Selection"
        useCachedConfig false
    }

let presentationModeConfig =
    config {
        content (element { id "render"; isCloseable false })
        useCachedConfig false
    }

let initial (dir : string) =
    let model = OpcSelectionViewerApp.initial dir in {
        appModel = model
        dockConfig = regularModeConfig
        provenance = ProvenanceApp.init model
        story = StoryApp.init
        animation = AnimationApp.init model
        renderControlSize = V2i.One
        directory = dir
    }

let update (model : Model) (act : Action) = 
    match act with
        | AnimationAction a ->
            model |> AnimationApp.update a

        | ProvenanceAction a ->
            let p = model.provenance |> ProvenanceApp.update model.story a
            let m = ProvenanceApp.restore model.appModel p

            // TODO: When the user goes to a different provenance state, we deselect
            // the currently selected slide. If this is useful is not clear at
            // this point.
            let updateStory =
                match a with
                    | Goto _ -> StoryApp.update DeselectSlide
                    | _ -> id

            model |> updateStory
                  |> Lens.set Model.Lens.provenance p
                  |> Lens.set Model.Lens.appModel m
        
        | StoryAction a ->
            model |> StoryApp.update a

        | SessionAction a ->
            model |> SessionApp.update a initial

        | AppAction a when not (Model.isAnimating model || Model.isPreview model) ->
            let s = OpcSelectionViewerApp.update model.appModel a
            let p = model.provenance |> ProvenanceApp.update model.story (Update (s, a))
            
            model |> Lens.set Model.Lens.appModel s
                  |> Lens.set Model.Lens.provenance p
                  |> StoryApp.update UpdateFrame

        | KeyDown Keys.Z ->
            model |> Lens.update Model.Lens.provenance (ProvenanceApp.update model.story Undo)

        | KeyDown Keys.R ->
            { model with dockConfig = regularModeConfig }

        | KeyDown Keys.P when Story.length model.story > 0 ->
            { model with dockConfig = presentationModeConfig }
                |> StoryApp.update StartPresentation

        | KeyDown Keys.Escape ->
            { model with dockConfig = regularModeConfig }
                |> StoryApp.update EndPresentation

        | KeyDown Keys.Right 
        | KeyDown Keys.Enter ->
            model |> StoryApp.update Forward

        | KeyDown Keys.Left
        | KeyDown Keys.Back ->
            model |> StoryApp.update Backward

        | RenderControlResized s ->
            { model with renderControlSize = s }

        | UpdateConfig cfg ->
            { model with dockConfig = cfg }

        | _ -> 
            model

let threads (model : Model) =
    
    // Thread pool for actual application
    let appThreads = model.appModel |> OpcSelectionViewerApp.threads 
                                    |> ThreadPool.map AppAction

    // Thread pool for animations
    let animationThreads = model |> AnimationApp.threads

    // Thread pool for story module                                                 
    let storyThreads = model |> StoryApp.threads
                             |> ThreadPool.map StoryAction

    [appThreads; animationThreads; storyThreads]
        |> ThreadPool.unionMany


let renderView (model : MModel) =
    onBoot "$(document).trigger('resize')" (
        body [ onResize RenderControlResized; onKeyDown KeyDown; onKeyUp KeyUp ] [
            model.appModel
                |> OpcSelectionViewerApp.renderView
                |> UI.map AppAction

            model |> StoryApp.overlayView
                  |> UI.map StoryAction
        ]
    )

let controlsView (model : MModel) =
    body [style "background-color:#1B1C1E"] [
        model.appModel
            |> OpcSelectionViewerApp.controlsView
            |> UI.map AppAction
    ]

let provenanceView (model : MModel) =
    body [] [
        model.provenance
            |> ProvenanceApp.view model.story
            |> UI.map ProvenanceAction
    ]

let storyboardView (model : MModel) =
    body [] [
        model |> StoryApp.storyboardView
              |> UI.map StoryAction
    ]

let presentationView (model : MModel) =
    let dependencies = Html.semui @ [
        { kind = Stylesheet; name = "presentationStyle"; url = "Presentation.css" }
    ]

    require (dependencies) (
        body [clazz "ui"] [
            Html.SemUi.accordion "Rendering" "options" true [
                model.appModel |> OpcSelectionViewerApp.renderingControlsView |> UI.map AppAction
            ]
        ]
    )

let view (model : MModel) =
    page (fun request ->
        match Map.tryFind "page" request.queryParams with
            | Some "render" -> 
                renderView model               
            | Some "controls" -> 
                controlsView model
            | Some "provenance" ->
                provenanceView model
            | Some "storyboard" ->
                storyboardView model
            | Some "presentation" ->
                presentationView model
            | Some other ->
                let msg = sprintf "Unknown page: %A" other
                body [] [
                    div [style "color:white; font-size:large; background-color:red; width:100%; height:100%"] [text msg]
                ]  
            | None ->
                div [
                    style "width:100%; height:100%; overflow:hidden"
                ] [
                    Incremental.div AttributeMap.Empty <| alist {
                        let! p = model.story.presentation
                        if not p then
                            yield SessionApp.view |> UI.map SessionAction
                    }

                    model.dockConfig |> docking [
                        style "width:100%; height:100%; overflow:hidden"
                        onLayoutChanged UpdateConfig
                    ]
                ]
    )
 
let app (dir : string) : App<Model,MModel,Action> =
    {
        unpersist = Unpersist.instance
        threads = threads
        initial = initial dir
        update = update
        view = view
    }
