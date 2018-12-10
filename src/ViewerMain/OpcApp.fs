module OpcSelectionViewerApp

open System
open System.IO
open Aardvark.UI
open Aardvark.Base
open Aardvark.Base.Ag
open Aardvark.Base.Incremental
open Aardvark.Base.Rendering
open Aardvark.SceneGraph
open Aardvark.SceneGraph.Semantics
open Aardvark.SceneGraph.Opc
open Aardvark.SceneGraph.SgPrimitives
open Aardvark.Rendering.Text
open Aardvark.UI.Primitives
open Aardvark.UI.Trafos
open FShade
open Aardvark.Base.Geometry
open Aardvark.Geometry
open ``F# Sg``

open OpcSelectionViewer
open OpcSelectionViewer.Picking

open SceneObjectHandling
open Aardvark.Application
open Aardvark.Base.DynamicLinkerTypes  
  
let update (model : OpcViewerModel) (msg : OpcViewerAction) =   
    match msg with
        | Camera m when model.pickingActive = false -> 
            { model with camera = FreeFlyController.update model.camera m; }
        | OpcViewerAction.KeyDown m ->
            match m with
                | Keys.LeftCtrl -> 
                    { model with pickingActive = true }
                | _ -> model
        | OpcViewerAction.KeyUp m ->
            match m with
                | Keys.LeftCtrl -> 
                    { model with pickingActive = false }
                | Keys.Delete ->
                    { model with picking = PickingApp.update model.picking (PickingAction.ClearPoints) }
                | Keys.Back ->
                    { model with picking = PickingApp.update model.picking (PickingAction.RemoveLastPoint) }            
                | _ -> model
        | PickingAction msg ->
            { model with picking = PickingApp.update model.picking msg }
        | _ -> model

let renderView (m : MOpcViewerModel) =

    let opcs = 
        m.opcInfos
            |> AMap.toASet
            |> ASet.map(fun info -> SceneObjectHandling.createSingleOpcSg m info)
            |> Sg.set
            |> Sg.effect [ 
                toEffect Shader.stableTrafo
                toEffect DefaultSurfaces.diffuseTexture       
            ]

    let scene = 
        [
            opcs
            PickingApp.view m.picking
        ] |> Sg.ofList

    FreeFlyController.controlledControl m.camera Camera m.frustum 
        (AttributeMap.ofList [ 
            style "width: 100%; height:100%"; 
            attribute "showFPS" "false";       // optional, default is false
            attribute "useMapping" "true"
            attribute "data-renderalways" "false"
            attribute "data-samples" "4"
            onKeyDown (OpcViewerAction.KeyDown)
            onKeyUp (OpcViewerAction.KeyUp)
            //onBlur (fun _ -> Camera FreeFlyController.Message.Blur)
    ]) scene

let controlsView (_ : MOpcViewerModel) =
    require Html.semui (
        body [style "width: 100%; height:100%; background: transparent";] [
            div[style "color:white"][text "UI COMES HERE"]
        ]
    )

let renderingControlsView (m : MOpcViewerModel) =
    body [] [
    ]

let initial dir =
    Serialization.registry.RegisterFactory (fun _ -> KdTrees.level0KdTreePickler)

    let phDirs = Directory.GetDirectories(dir) |> Array.head |> Array.singleton

    let patchHierarchies =
        [ 
            for h in phDirs do
            yield PatchHierarchy.load Serialization.binarySerializer.Pickle Serialization.binarySerializer.UnPickle (h |> OpcPaths)
        ]    

    let box = 
        patchHierarchies 
            |> List.map(fun x -> x.tree |> QTree.getRoot) 
            |> List.map(fun x -> x.info.GlobalBoundingBox)
            |> List.fold (fun a b -> Box3d.Union(a, b)) Box3d.Invalid
      
    let opcInfos = 
        [
            for h in patchHierarchies do
            
            let rootTree = h.tree |> QTree.getRoot

            yield {
                patchHierarchy = h
                kdTree         = KdTrees.loadKdTrees' h Trafo3d.Identity true ViewerModality.XYZ Serialization.binarySerializer
                localBB        = rootTree.info.LocalBoundingBox 
                globalBB       = rootTree.info.GlobalBoundingBox
                neighborMap    = HMap.empty
            }
        ]
        |> List.map (fun info -> info.globalBB, info)
        |> HMap.ofList      
                      
    let camState = { FreeFlyController.initial with view = CameraView.lookAt (box.Center) V3d.OOO V3d.OOI; }

    { 
        camera             = camState
        frustum            = Frustum.perspective 60.0 0.1 50000.0 1.0
        fillMode           = FillMode.Fill                    
        patchHierarchies   = patchHierarchies          
                    
        threads            = FreeFlyController.threads camState |> ThreadPool.map Camera
      
        pickingActive      = false
        opcInfos           = opcInfos
        picking            = { PickingModel.initial with pickingInfos = opcInfos }
        sceneHit           = V3d.Zero
    }

let threads (model : OpcViewerModel) =
    model.threads
       