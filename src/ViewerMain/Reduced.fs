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
    | Unknown 

    override x.ToString () =
        match x with
            | AddPoint _ -> "A"
            | RemovePoint _ -> "R"
            | Unknown -> ""

type OState = AppModel

[<DomainType; CustomEquality; NoComparison>]
type State =
    { pickPoints : V3d plist }

    override x.GetHashCode () =
        x.pickPoints |> PList.toList |> hash

    override x.Equals y =
        match y with
            | :? State as y -> PList.toList x.pickPoints = PList.toList y.pickPoints
            | _ -> false


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

    let equal (a : OCameraView) (b : OCameraView) =
        (a |> create) = (b |> create)

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Message =
    
    let create (current : State) (next : State) (msg : OMessage) =
        match msg with
            | PickingAction (Pick _) ->
                AddPoint
            | _ ->
                let c = PList.count current.pickPoints
                let n = PList.count next.pickPoints

                if n > c then
                    AddPoint
                elif n < c then
                    RemovePoint
                else
                    Unknown

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module State =
    
    let create (s : OState) =
        { pickPoints = s.picking.intersectionPoints }
        
    let restore (current : OState) (s : State) =
        { current with picking = { current.picking with intersectionPoints = s.pickPoints } }

    let pickPoints (s : State) = s.pickPoints