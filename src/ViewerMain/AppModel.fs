namespace Model

open Aardvark.Base.Rendering
open OpcSelectionViewer

type AppAction = OpcViewerAction
type AppModel = OpcViewerModel

type RenderingParams = {
    fillMode : FillMode
}