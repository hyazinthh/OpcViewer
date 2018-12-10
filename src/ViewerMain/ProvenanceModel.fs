namespace Provenance

open System
open Aardvark.Base
open Aardvark.Base.Incremental

open Model

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
    state : Reduced.State
    message : Option<Reduced.Message>
}

[<DomainType>]
type Provenance = {
    tree : ZTree<Node>
    highlight : NodeId option
    preview : ZTree<Node> option
}

type ProvenanceAction =
    | Update            of AppModel * AppAction
    | Goto              of NodeId
    | Undo
    | SetHighlight      of NodeId
    | RemoveHighlight
    | MouseEnter        of NodeId
    | MouseLeave

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Node =

    let create (s : Reduced.State) (m : Reduced.Message option) =
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