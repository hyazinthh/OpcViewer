namespace Animation

open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.UI.Animation

[<DomainType>]
type Animation = {
    model : AnimationModel
    savedView : CameraView
}

