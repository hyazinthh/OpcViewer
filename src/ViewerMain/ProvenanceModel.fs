namespace Provenance

open System
open Aardvark.Base
open Aardvark.Base.Incremental

open Provenance.Reduced

type NodeId = 
    private NodeId of Guid with

    static member generate () =
        NodeId (Guid.NewGuid ())

    static member ofGuid (v : Guid) =
        NodeId v

    static member parse (s : string) =
        s |> Guid.Parse |> NodeId

    static member tryParse (s : string) =
        try
            Some (s |> Guid.Parse |> NodeId)
        with
            | _ -> None

    override x.ToString () =
        let (NodeId v) = x in string v

[<DomainType>]
type Node = {
    [<PrimaryKey>]
    id : NodeId
    state : State
    message : Option<Message>
}

[<DomainType>]
type Provenance = {
    tree : ZTree<Node>
    highlight : NodeId option
    hovered : ZTree<Node> option
}

type ProvenanceAction =
    | Update            of State * Message
    | UpdateCamera      of CameraView
    | Goto              of NodeId
    | SetHighlight      of NodeId
    | RemoveHighlight
    | MouseEnter        of NodeId
    | MouseLeave

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Node =

    let create (s : State) (m : Message option) =
        { id = NodeId.generate ()
          state = s
          message = m }

    let state (n : Node) = n.state

    let message (n : Node) = n.message

    let id (n : Node) = n.id

    let properties (n : Node) = [ 
        let (NodeId id) = n.id
        yield "id", (string id)

        match n.message with
            | Some m -> yield "msg", (string m)
            | None -> ()
    ]

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Provenance =

    let current (p : Provenance) =
        ZTree.value p.tree

    let state =
        current >> Node.state