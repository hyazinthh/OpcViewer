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
    | Unknown 

    override x.ToString () =
        match x with
            | Unknown -> ""

type OState = AppModel

[<DomainType>]
type State = {
    pickPoints : V3d plist
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

    let equal (a : OCameraView) (b : OCameraView) =
        (a |> create) = (b |> create)

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Message =
    
    let create : (OMessage -> Message) = function
        | _ -> Unknown

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module State =
    
    let create (s : OState) =
        { pickPoints = s.picking.intersectionPoints }
        
    let restore (current : OState) (s : State) =
        { current with picking = { current.picking with intersectionPoints = s.pickPoints } }

    let pickPoints (s : State) = s.pickPoints