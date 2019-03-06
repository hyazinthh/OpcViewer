namespace Preview

open Aardvark.Base
open Aardvark.Base.Incremental

open Model

[<DomainType>]
type Mode =
    | View
    | Model

[<DomainType>]
type Preview = {
    model : AppModel
    kind : Mode hset
}

type PreviewAction =
    | Start of Preview
    | Stop

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Preview =

    let view (model : AppModel) =
        { model = model; kind = HSet.ofList [View] }

    let model (model : AppModel) =
        { model = model; kind = HSet.ofList [Model] }

    let full (model : AppModel) =
        { model = model; kind = HSet.ofList [View; Model] }

    let is (mode : Mode) (preview : Preview) =
        preview.kind |> HSet.contains mode