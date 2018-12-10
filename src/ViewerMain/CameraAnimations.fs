namespace Aardvark.UI.Animation

open Aardvark.Base
open Aardvark.UI.Animation

[<AutoOpen>]
module CameraAnimationsExtensions =

    module CameraAnimations =

        let interpolate (destination : CameraView) (duration : RelativeTime) (name : string) =
            {
                (CameraAnimations.initial name) with
                    sample = fun (localTime, _) (state : CameraView) -> // given the state and t since start of the animation, compute a state and the cameraview
                        if localTime < duration then
                            let t = localTime / duration

                            let vec      = destination.Location - state.Location
                            let location' = state.Location + vec * t

                            let a = Rot3d.FromFrame (state.Right, state.Up, state.Backward)
                            let b = Rot3d.FromFrame (destination.Right, destination.Up, destination.Backward)
                            let orientation = (a, b, t) |> Ipol.SlerpShortest |> M44d.Rotation

                            let forward' = -orientation.C2.XYZ
                            let right' = orientation.C0.XYZ
                            let up' = orientation.C1.XYZ

                            let view = CameraView (state.Sky, location', forward', up', right')

                            Some (state,view)
                        else None
            }