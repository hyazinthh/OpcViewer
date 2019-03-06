module PreviewApp

open Aardvark.Base

open View
open Preview
open Model

let update (msg : PreviewAction) (model : Model) =

    match msg with
        | Start mode ->
            model |> Lens.set (Model.Lens.inner |. InnerModel.Lens.preview) (Some mode)
                  |> if Preview.is View mode then
                         ViewApp.update Preview
                     else
                         id
                
        | Stop ->
            model |> Lens.set (Model.Lens.inner |. InnerModel.Lens.preview) None
                  |> Lens.update (Model.Lens.inner |. InnerModel.Lens.current) (AppModel.setViewParams model.view.state)
                  |> ViewApp.update Move