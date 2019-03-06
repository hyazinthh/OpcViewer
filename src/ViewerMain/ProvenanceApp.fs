module ProvenanceApp

open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.UI

open Model
open Provenance
open Provenance.Reduced
open Story

[<AutoOpen>]
module private Helpers =

    module Rules =

        let checkMessage (msg : Message) (input : Decision<Node ztree>) =
            input |> Decision.map (fun tree ->
                    if msg = Unknown then
                        Decided tree
                    else
                        Undecided tree 
            )

        let checkStateChanged (state : State) (input : Decision<Node ztree>) =
            input |> Decision.map (fun tree ->
                    if tree.Value.state = state then
                        Decided tree
                    else
                        Undecided tree 
            )

        let checkParent (state : State) (input : Decision<Node ztree>) =
            input |> Decision.map (fun tree ->
                match tree.Parent with
                    | Some p when (p.Value.state = state) ->
                        Decided p
                    | _ ->
                        Undecided tree
            ) 

        let checkChildren (state : State) (msg : Message) (input : Decision<Node ztree>) =
            input |> Decision.map (fun tree ->
                let c =
                    tree |> ZTree.filterChildren (fun n ->
                        (n.state = state) && (n.message = Some msg)
                    )
            
                if List.length c > 1 then
                    Log.warn "Multiple identical children in provenance graph."

                match c with
                    | [] -> Undecided tree
                    | t::_ -> Decided t
            ) 

        let coalesceWithCurrent (story : Story) (state : State) (msg : Message) (input : Decision<Node ztree>) =
            input |> Decision.map (fun tree ->
                let node = tree.Value

                let coal = 
                    node |> Node.message
                         |> Option.contains msg
                         |> (&&) (story |> Story.isNodeReferenced node true |> not)
                         |> (&&) (ZTree.isLeaf tree)                

                match coal with
                    | true ->
                        let v = { node with state = state }
                        Decided (tree |> ZTree.set v)
                    | false ->
                        Undecided tree
            )

        let coalesceWithChild (story : Story)  (state : State) (msg : Message) (input : Decision<Node ztree>) =
            input |> Decision.map (fun tree ->
                let c =
                    tree |> ZTree.filterChildren (fun n ->
                                (n.message = Some msg) && (story |> Story.isNodeReferenced n true |> not)
                            )
                            |> List.filter ZTree.isLeaf

                if List.length c > 1 then
                    Log.warn "Multiple leaf children to coalesce in provenance graph."

                match c with
                    | [] -> 
                        Undecided tree
                    | t::_ ->
                        let v = { t.Value with state = state }
                        Decided (t |> ZTree.set v)
            )

        let appendNew (state : State) (msg : Message) (input : Decision<Node ztree>)=
            input |> Decision.map (fun tree ->
                tree |> ZTree.insert (Node.create state (Some msg)) |> Decided
            )

 [<AutoOpen>]
 module private Events =
    // Fired when a node is clicked 
    let onNodeClick (cb : NodeId -> 'msg) =
        onEvent "onnodeclick" [] (List.head >> Pickler.unpickleOfJson >> NodeId.parse >> cb)

    // Fired when the mouse enters a node
    let onNodeMouseEnter (cb : NodeId -> 'msg) =
        onEvent "onnodemouseenter" [] (List.head >> Pickler.unpickleOfJson >> NodeId.parse >> cb)

    // Fired when the mouse leaves a node
    let onNodeMouseLeave (cb : unit -> 'msg) =
        onEvent "onnodemouseleave" [] (ignore >> cb)

let init (state : State) =
    { tree = Node.create state None |> ZTree.single
      highlight = None
      hovered = None }

let rec update (story : Story) (msg : ProvenanceAction) (p : Provenance) =
    match msg with
        | Update (next, msg) ->
            let t =
                Undecided p.tree
                    |> Rules.checkMessage msg
                    |> Rules.checkStateChanged next
                    |> Rules.checkParent next
                    |> Rules.checkChildren next msg
                    |> Rules.coalesceWithCurrent story next msg
                    |> Rules.coalesceWithChild story next msg
                    |> Rules.appendNew next msg
                    |> Decision.get

            { p with tree = t }

        | UpdateCamera c ->
            let next = { Provenance.state p with camera = Some c }
            let msg = Update (next, Camera c)

            p |> update story msg

        | Goto id ->
            p.tree |> ZTree.root
                   |> ZTree.find (fun n -> n.id = id)
                   |> fun t -> { p with tree = t }

        | MouseEnter id ->
            let t = p.tree |> ZTree.root
                           |> ZTree.find (fun n -> n.id = id)

            { p with hovered = Some t }

        | MouseLeave ->
            { p with hovered = None }

        | SetHighlight id ->
            { p with highlight = Some id }

        | RemoveHighlight ->
            { p with highlight = None }

let view (camera : IMod<CameraView>)(s : MStory) (p : MProvenance) =
    let dependencies = [
        { kind = Script; name = "d3"; url = "http://d3js.org/d3.v5.min.js" }
        { kind = Stylesheet; name = "provenanceStyle"; url = "Provenance.css" }
        { kind = Script; name = "provenanceScript"; url = "Provenance.js" }
    ]

    let colorSelected = C3d (0.75, 0.95, 0.18);
    let colorHovered = C3d (0.2, 0.8, 0.99);

    let dropShadow (name : string) (color : C3d) =
        let colorMatrix =
            sprintf  "%f 0 0 0 0, 0 %f 0 0 0, 0 0 %f 0 0, 0 0 0 1 0" color.R color.G color.B

        Svg.filter [
            clazz name
            attribute "x" "-50%"
            attribute "y" "-50%"
            attribute "width" "200%"
            attribute "height" "200%"
        ] [
            Svg.feColorMatrix [
                attribute "type" "matrix"
                attribute "result" "whiteOut"
                attribute "in" "SourceGraphic"
                attribute "values" "0 0 0 0 1, 0 0 0 0 1, 0 0 0 0 1, 0 0 0 1 0"
            ]

            Svg.feColorMatrix [
                attribute "type" "matrix"
                attribute "result" "colorOut"
                attribute "in" "whiteOut"
                attribute "color-interpolation-filters" "sRGB"
                attribute "values" colorMatrix
            ]

            Svg.feGaussianBlur [
                attribute "result" "blurOut"
                attribute "in" "colorOut"
                attribute "stdDeviation" "2"
            ]

            Svg.feBlend [
                attribute "in" "SourceGraphic"
                attribute "in2" "blurOut"
                attribute "mode" "normal"
            ]
        ]

    let cameraMenu =
        Incremental.div (AttributeMap.ofAMap <| amap {
            let! c = camera

            let! modified = p.Current |> Mod.map (fun p ->
                Some c <> (p |> Provenance.state |> State.camera)
            )

            yield clazz <| "ui camera icon toggle button" + if modified then "" else " disabled"

            let! c = camera
            yield onClick (fun _ -> UpdateCamera c)

        }) <| AList.ofList [
            i [clazz "camera icon"] []
        ]
        
    let provenanceData = adaptive {
        let! t = p.tree
        let! h = p.highlight |> Mod.map (Option.map string >> Option.defaultValue "")

        // TODO: Dependency on the whole story struct is an overkill, can be optimized.
        let! s = s.Current

        let props n = 
            ("isReferenced", s |> Story.isNodeReferenced n false |> string) :: (Provenance.Node.properties n)
        
        // TODO: It might be possible to increase performance by handling
        // the highlight property with a separate channel that does not trigger the
        // recomputation of the whole tree
        let json = t.Root.ToJson props
        return sprintf @"{ ""current"" : ""%A"" ,  ""highlight"" : ""%s"" , ""tree"" : %s }" t.Value.id h json
    }

    let updateChart = "update(__DATA__)"

    div [ 
        clazz "provenanceView"
        onNodeClick Goto
        onNodeMouseEnter MouseEnter
        onNodeMouseLeave (fun _ -> MouseLeave)
    ] [
        require dependencies (
            onBootInitial "provenanceData" provenanceData updateChart (
                div [] [
                    Svg.svg [ clazz "rootSvg" ] [
                        Svg.defs [] [
                            dropShadow "shadowSelected" colorSelected
                            dropShadow "shadowHovered" colorHovered
                        ]

                        Svg.g [ clazz "linkLayer" ] []
                        Svg.g [ clazz "nodeLayer" ] []
                    ]

                    cameraMenu
                ]
            )
        )
    ]