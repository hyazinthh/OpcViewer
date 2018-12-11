namespace OpcSelectionViewer.Picking

open Aardvark.UI
open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.Geometry
open OpcSelectionViewer
open OpcSelectionViewer.KdTrees
open Aardvark.SceneGraph.Opc

type PickingAction =
  | Enable
  | Disable
  | Pick       of Box3d * SceneHit  
  | HitSurface of Box3d * SceneHit    
  | RemoveLastPoint
  | ClearPoints

type BoxNeighbors = {
  neighbors : List<Box3d> 

}
  

[<DomainType>]
type OpcData = {
  [<NonIncremental>]
  patchHierarchy : PatchHierarchy
  kdTree         : hmap<Box3d, Level0KdTree>
  neighborMap    : hmap<Box3d, BoxNeighbors>

  localBB        : Box3d
  globalBB       : Box3d
}

[<DomainType>]
type PickingModel = {
  active               : bool
  currentPoint         : V3d
  pickingInfos         : hmap<Box3d, OpcData>
  hitPointsInfo        : hmap<V3d, Box3d>
  intersectionPoints   : plist<V3d>  
}  

module PickingModel =

  let initial = 
    {
      active             = false
      currentPoint       = V3d.Zero
      pickingInfos       = HMap.empty
      hitPointsInfo      = HMap.empty
      intersectionPoints = PList.empty
    }