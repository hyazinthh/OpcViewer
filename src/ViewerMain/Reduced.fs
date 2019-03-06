namespace Provenance.Reduced

open Aardvark.Base
open Aardvark.Base.Incremental

open Model
open OpcSelectionViewer
open OpcSelectionViewer.Picking

type OCameraView = CameraView

// TODO: We need to redefine this because we want structural equality but CameraView is a class :/
[<DomainType>]
type CameraView = {
    sky : V3d
    location : V3d
    forward : V3d
    up : V3d
    right : V3d
}

type OMessage = AppAction

[<DomainType>]
type Message =
    | AddPoint
    | RemovePoint
    | Camera of CameraView
    | Unknown 

    override x.ToString () =
        match x with
            | AddPoint -> "A"
            | RemovePoint -> "R"
            | Camera _ -> "C"
            | Unknown -> ""

type OState = AppModel

[<DomainType>]
type State = {
    camera : CameraView option
    pickPoints : V3d list
}

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module CameraView =
    
    let create (v : OCameraView) =
        { sky = v.Sky
          location = v.Location
          forward = v.Forward
          up = v.Up
          right = v.Right }

    let restore (v : CameraView) =
        OCameraView (v.sky, v.location, v.forward, v.up, v.right)

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Message =
    
    let create (current : State) (next : State) (msg : OMessage) =
        match msg with
            | _ ->
                let c = List.length current.pickPoints
                let n = List.length next.pickPoints

                if n > c then
                    AddPoint
                elif n < c then
                    RemovePoint
                else
                    Unknown

    let isCamera : OMessage -> bool = function
        | OMessage.Camera _ -> true
        | _ -> false

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module State =
    
    let create (camera : CameraView option) (s : OState) =
        { camera = camera
          pickPoints = PList.toList s.picking.intersectionPoints }

    let restore (current : OState) (s : State) =
        let camera =
            match s.camera with
                | None -> current.camera
                | Some c -> { current.camera with view = CameraView.restore c }

        { current with camera = camera
                       picking = { current.picking with intersectionPoints = PList.ofList s.pickPoints } }

    let camera (s : State) = s.camera

    let pickPoints (s : State) = s.pickPoints