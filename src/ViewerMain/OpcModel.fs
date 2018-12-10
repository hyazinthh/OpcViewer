namespace OpcSelectionViewer

open Aardvark.Base
open Aardvark.Base.Rendering
open Aardvark.Base.Incremental
open Aardvark.SceneGraph.Opc
open Aardvark.UI.Primitives
open Aardvark.Application

open OpcSelectionViewer.Picking

type OpcViewerAction =
  | Camera           of FreeFlyController.Message
  | KeyUp            of key : Keys
  | KeyDown          of key : Keys
  | PickingAction    of PickingAction

[<DomainType>]
type OpcViewerModel =
    {
        camera               : CameraControllerState
        frustum              : Frustum
        fillMode             : FillMode                                
        [<NonIncremental>]
        patchHierarchies     : list<PatchHierarchy>        
        
        opcInfos             : hmap<Box3d, OpcData>
        threads              : ThreadPool<OpcViewerAction>
        picking              : PickingModel
        pickingActive        : bool

        sceneHit             : V3d
    }

   