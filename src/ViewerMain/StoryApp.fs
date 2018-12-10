module StoryApp

open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.UI
open Aardvark.UI.Animation

open Story
open Model
open Provenance
open Annotations
open Thumbnail

[<AutoOpen>]
module private Helpers =

    module Annotations =
        // Updates the annotations
        let update (msg : AnnotationAction) (model : Model) =

            let updateContent = function
                | FrameContent (n, p, a) ->
                    FrameContent (n, p, a |> AnnotationApp.update msg)
                | x -> x

            let sel = model.story |> Story.trySelected
                                  |> Option.map (Lens.update (Selection.Lens.modified |. Slide.Lens.content) updateContent)

            model |> Lens.set (Model.Lens.story |. Story.Lens.selected) sel

    // Creates a new frame
    let frame (provenance : Provenance) (presentation : PresentationParams) =
        { id = SlideId.generate ()
          content = FrameContent (Provenance.current provenance, presentation, AnnotationApp.init)
          thumbnail = ThumbnailApp.empty }

    // Updates the frame content of the selected slide
    let updateFrame (model : Model) =

        let update (s : Selection) =
            match s.modified.content with
                | TextContent _ -> model
                | FrameContent (_, p, a) ->
                    let s =
                        let n = Provenance.current model.provenance
                        let p = if Model.isAnimating model then p else Model.getPresentation model
                        let cont = FrameContent (n, p, a)

                        s |> Lens.set (Selection.Lens.modified |. Slide.Lens.content) cont
                          |> Some

                    model |> Lens.set (Model.Lens.story |. Story.Lens.selected) s
        
        model.story |> Story.trySelected
                    |> Option.map update
                    |> Option.defaultValue model

    // Commits changes to the story
    let commit (model : Model) =

        let updateThumbnail (s : Selection) =
            model |> if Model.isAnimating model then id else ThumbnailApp.syncRequest s.current.id

        model.story |> Story.trySelected
                    |> Option.map updateThumbnail
                    |> Option.defaultValue model
                    |> Lens.update Model.Lens.story Story.commit

    // Restores the model from the selected slide
    let restore (animate : bool) (model : Model) =

        let restoreContent = function
            | FrameContent (node, presentation, _) ->
                let p = model.provenance |> ProvenanceApp.update model.story (Goto node.id)
                let m = ProvenanceApp.restore model.appModel p

                model |> Lens.set Model.Lens.provenance p
                      |> Lens.set Model.Lens.appModel m
                      |> Model.setPresentation presentation
                      |> if animate then AnimationApp.animate 0.5 model else id
            | _ -> 
                model

        model.story |> Story.trySelected
                    |> Option.map (Selection.current >> Slide.content >> restoreContent)
                    |> Option.defaultValue model

    let getFrustum (model : MModel) =
        model.appModel.frustum

    let getView (model : MModel) =
        model.appModel.camera.view

    let getViewFromPresentation (p : MPresentationParams) =
        p.view.Current |> Mod.map Reduced.CameraView.restore

    let getViewProjTrafo (size : IMod<V2i>) (frustum : IMod<Frustum>) (view : IMod<CameraView>) = 
        let cfg = RenderControlConfig.standard

        adaptive {
            let! s = size
            let! f = frustum
            let! v = view

            return f |> cfg.adjustAspect s
                     |> Camera.create v
                     |> Camera.viewProjTrafo
        }
        
    let getViewProjTrafoFromModel (model : MModel) =
        getViewProjTrafo model.renderControlSize (getFrustum model) (getView model)

    let getSceneHit (model : MModel) =
        model.appModel.sceneHit

 [<AutoOpen>]
 module private Events =
    // Fired when a slide is dragged and dropped
    let onSlideMove (cb : SlideId * SlideId option * SlideId option -> 'msg) =
        onEvent "onslidemove" [] (fun args ->
            let x = args |> List.toArray |> Array.map (Pickler.unpickleOfJson >> SlideId.tryParse)
            cb (x.[0].Value, x.[1], x.[2])
        )

    // TODO: Remove this once media has fixed its onMouseLeave
    let onMouseLeave (cb : V2i -> 'msg) =
        onEvent "onmouseleave" ["{ X: event.clientX, Y: event.clientY  }"] (List.head >> Pickler.unpickleOfJson >> cb)

    let onClick' (cb : unit -> 'msg list) =
        onEvent' "onclick" [] (ignore >> cb >> Seq.ofList)

    let onThumbnailLoaded (cb : V2i -> 'msg) =
        onEvent "onthumbnailloaded" [] (List.head >> Pickler.unpickleOfJson >> cb)

let init = {
    slides = PList.empty
    selected = None
    showAnnotations = false
    thumbnailRequests = HSet.empty
    presentation = false
}

let update (msg : StoryAction) (model : Model) =
    match msg with
        | UpdateFrame ->
            model |> updateFrame

        | AnnotationAction a ->
            model |> Annotations.update a

        | Forward ->
            model |> Lens.update Model.Lens.story Story.forward
                  |> restore true

        | Backward ->
            model |> Lens.update Model.Lens.story Story.backward
                  |> restore true

        | Commit ->
            model |> commit

        | StartPresentation ->
            model |> Lens.update Model.Lens.story Story.startPresentation
                  |> restore true

        | EndPresentation ->
            model |> Lens.update Model.Lens.story Story.endPresentation

        | SelectSlide id -> 
            model |> Lens.update Model.Lens.story (Story.selectById <| Some id)
                  |> restore true

        | RemoveSlide id ->
            model |> Lens.update Model.Lens.story (Story.removeById id)

        | MoveSlide (id, l, r) ->
            let story = 
                if l.IsSome then
                    Story.moveAfterById id l.Value
                else
                    Story.moveBeforeById id r.Value

            model |> Lens.update Model.Lens.story story

        | AddFrameSlide before ->
            let slide = frame model.provenance (Model.getPresentation model)
            let add = 
                match before with
                    | None -> Story.append slide
                    | Some id -> Story.insertBeforeById slide id

            model |> Lens.update Model.Lens.story add
                  |> ThumbnailApp.request slide.id

        | AddTextSlide _ ->
            model

        | DuplicateSlide id ->
            model |> Lens.update Model.Lens.story (Story.duplicateById id)
                  |> restore true

        | DeselectSlide ->
            model |> Lens.update Model.Lens.story (Story.select None)

        | MouseEnterSlide id ->
            let slide = model.story |> Story.findById id
            match slide.content with
                | FrameContent (n, _, _) ->
                    model |> Lens.update Model.Lens.provenance (ProvenanceApp.update model.story <| SetHighlight n.id)
                | _ ->
                    model

        | MouseLeaveSlide ->
            model |> Lens.update Model.Lens.provenance (ProvenanceApp.update model.story RemoveHighlight)

        | ThumbnailAction (id, a) ->
            model |> ThumbnailApp.update id a

        | ToggleAnnotations ->
            model |> Lens.update (Model.Lens.story |. Story.Lens.showAnnotations) not

let threads (model : Model) =
    model |> ThumbnailApp.threads
          |> ThreadPool.map ThumbnailAction

let overlayView (model : MModel) =

    // We don't want buttons to take away the focus from
    // the textareas of the labels
    let preventBlur (x : DomNode<'a>) =
        onBoot "$('#__ID__').on('mousedown', function (event) { event.preventDefault(); });" x

    let overlay (a : MAnnotations) =
        let focus = a.focus |> Mod.map Option.isSome

        div [clazz "frame"] [
            i [clazz "huge camera icon"] []

            Incremental.div (AttributeMap.ofAMap <| amap {
                let! hidden = model.story.selected |> Mod.bind (fun s ->
                    let s = s.Value
                    Mod.map2 (=) s.current.Current s.modified.Current
                )

                yield clazz <| "confirm buttons" + if hidden then " hidden" else ""

            }) <| AList.ofList [
                i [
                    clazz "huge checkmark icon"
                    onClick' (fun _ -> [Commit; DeselectSlide])
                ] []

                i [
                    clazz "huge remove icon"
                    onClick (fun _ -> DeselectSlide)
                ] []
            ]

            Incremental.div (AttributeMap.ofList [clazz "annotation menu"]) <|
                alist {
                    let! hidden = model.story.showAnnotations |> Mod.map not

                    yield div [
                        clazz ("ui icon toggle button" + if hidden then "" else " active")
                        onClick (fun _ -> ToggleAnnotations)
                    ] [
                        i [clazz "comments icon"] []
                    ]

                    if not hidden then
                        yield div [ clazz "ui vertical buttons" ] [
                            preventBlur (
                                Incremental.div (AttributeMap.ofAMap <| amap {
                                    let! focus = focus
                                    yield clazz <| "ui icon button" + if focus then "" else " disabled"
                                }) <| AList.ofList [
                                    i [clazz "font icon"] []
                                ]
                            )

                            preventBlur (
                                Incremental.div (AttributeMap.ofAMap <| amap {
                                    let! focus = focus
                                    let! targeting = a.targeting

                                    yield clazz <| "ui icon button" +
                                                   (if focus then "" else " disabled") +
                                                   (if targeting then " active" else "")

                                    yield onClick (fun _ -> Target |> AnnotationAction)
                                }) <| AList.ofList [
                                    i [clazz "flag icon"] []
                                ]
                            )

                            preventBlur (
                                div [
                                    clazz "ui icon button"
                                    onClick (fun _ -> Add |> AnnotationAction)
                                ] [
                                    i [clazz "add icon"] []
                                ]
                            )
                        ]
                }
        ]

    let dependencies = Html.semui @ [
        { kind = Stylesheet; name = "overlayStyle"; url = "Overlay.css" }
    ]

    require (dependencies) (
        Incremental.div (AttributeMap.ofList [
            clazz "render overlay"
        ]) <| alist {
            let! selected = model.story.selected
            let! show = model.story.showAnnotations
            let! presentation = model.story.presentation

            if selected.IsSome then
                let! cont = selected.Value.modified.content

                match cont with
                    | MFrameContent (_, _, a) ->
                        if show || presentation then
                            let viewport = model.renderControlSize
                            let vp = getViewProjTrafoFromModel model
                            let sceneHit = getSceneHit model

                            yield a |> AnnotationApp.view viewport vp sceneHit presentation
                                    |> UI.map AnnotationAction

                        if not presentation then
                            yield overlay a
                    | _ -> ()
        }
    )

let storyboardView (model : MModel) =
    let dependencies = Html.semui @ [
        { kind = Stylesheet; name = "storyboardStyle"; url = "Storyboard.css" }
        { kind = Script; name = "storyboardScript"; url = "Storyboard.js" }
    ]

    let disableClickPropagation x =
        onBoot "disableClickPropagation($('#__ID__'))" x

    let addSlideButton (before : SlideId option) =
        onBoot "initAddButton($('#__ID__'))" (
            div [clazz "add button"] [
                div [clazz "ui icon button first"] [
                    i [clazz "add icon"] []
                ]
                div [clazz "ui vertical buttons second"] [
                    div [clazz "ui button"; onClick (fun _ -> AddFrameSlide before)] [
                        i [clazz "camera icon"] []
                        text "Frame"                        
                    ]
                    div [clazz "ui button"; onClick (fun _ -> AddTextSlide before)] [
                        i [clazz "list icon"] []
                        text "Text"
                    ]
                ]
            ]
        )   

    let mkCollapsingFrame (model : MModel) (slide : MSlide) =
        // TODO: Optimize! This depends on the whole story record!
        let prev = Mod.map2 (fun story slide ->
                        story |> Story.leftOf slide |> Option.map (Slide.id >> string)
                   ) model.story.Current slide.Current

        onBoot "initCollapsingFrame($('#__ID__'))" (
            Incremental.div (AttributeMap.ofAMap <| amap {
                yield clazz "collapsing preview frame"

                let! id = slide.id
                yield attribute "data-right" (string id)

                let! prev = prev
                if prev.IsSome then 
                    yield attribute "data-left" prev.Value
                    
            }) <| alist {
                let! id = slide.id
                yield addSlideButton (Some id)
            }
        )

    let mkStaticFrame (model : MModel) =
        // TODO: Optimize! This depends on the whole story record!
        let last = model.story.Current |> Mod.map (fun s ->
                        s |> Story.last |> Option.map (Slide.id >> string)
                   )    

        onBoot "setupDropEvents($('#__ID__'))" (
            Incremental.div (AttributeMap.ofAMap <| amap {
                yield clazz "static preview frame"

                let! last = last
                if last.IsSome then
                    yield attribute "data-left" last.Value

            }) <| AList.ofList [
                yield addSlideButton None
            ]
        )
        
    let mkSlide (model : MModel) (slide : MSlide)  =
        let selected = adaptive {
            let! id = slide.id
            let! sel = model.story.selected

            match sel with
                | None -> return false
                | Some x -> return! x.current.id |> Mod.map ((=) id)
        }

        let highlighted = adaptive {
            let! cont = slide.content
            let! preview = model.provenance.preview

            match cont with
                | MFrameContent (n, _, _) ->
                    let! id = n.id
                    return preview |> Option.map (fun t -> id = t.Value.id)
                                   |> Option.defaultValue false
                | _ ->
                    return false
        }

        onBoot "setupDragEvents($('#__ID__'))" (
            Incremental.div (AttributeMap.ofAMap <| amap {
                let! id = slide.id
                let! selected = selected
                let! highlighted = highlighted

                yield clazz <| "frame" + if selected then " selected" else ""
                                       + if highlighted then " highlighted" else ""
                yield attribute "data-slide" <| string id
                yield onClick <| if selected then (fun _ -> DeselectSlide) else (fun _ -> SelectSlide id)
                yield onMouseEnter (fun _ -> MouseEnterSlide id)
                yield onMouseLeave (fun _ -> MouseLeaveSlide)
                yield onThumbnailLoaded (fun s -> ThumbnailAction (id, Resized s))

            }) <| AList.ofList [
                Incremental.div (AttributeMap.ofList [clazz "thumbnail"]) <| alist {
                    let thumbData = slide.thumbnail.data |> Mod.map string
                    let updateThumb = "setupThumbnail($('#__ID__'), __DATA__)"

                    yield onBootInitial "thumbData" thumbData updateThumb (
                        img []
                    )

                    // TODO: May wanna move this into another Incremental.div
                    // to prevent the thumbnail from constantly updating
                    let! content = slide.content

                    match content with
                        | MFrameContent (_, p, a) ->
                            let viewport = slide.thumbnail.displaySize
                            let vp = getViewProjTrafo viewport (getFrustum model) (getViewFromPresentation p)

                            let sceneHit = getSceneHit model

                            yield a |> AnnotationApp.view viewport vp sceneHit true
                                    |> UI.map AnnotationAction
                        | _ -> ()
                }
            
                disableClickPropagation (
                    Incremental.div (AttributeMap.ofAMap <| amap {
                        let! id = slide.id
                        yield clazz "ui icon remove slide button"
                        yield onClick (fun _ -> RemoveSlide id)

                    }) <| AList.ofList [
                        i [clazz "remove icon"] []
                    ]
                )

                disableClickPropagation (
                    Incremental.div (AttributeMap.ofAMap <| amap {
                        let! id = slide.id
                        yield clazz "ui icon duplicate slide button"
                        yield onClick (fun _ -> DuplicateSlide id)

                    }) <| AList.ofList [
                        i [clazz "copy icon"] []
                    ]
                )

                Incremental.div (AttributeMap.ofList [clazz "ui floating blue label"]) <| alist {
                    // TODO: Optimize! This depends on the whole story record!
                    let! s = slide.Current
                    let! index = model.story.Current |> Mod.map (Story.findIndex s)
                    yield text (string (index + 1))
                }
            ]
        )

    require dependencies (
        Incremental.div (AttributeMap.ofList [ 
            clazz "storyboard"
            onSlideMove MoveSlide 
        ]) <| alist {
            for s in model.story.slides do
                yield s |> mkCollapsingFrame model
                yield s |> mkSlide model

            yield mkStaticFrame model
        }
    )