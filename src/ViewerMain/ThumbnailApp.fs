module ThumbnailApp

open System
open System.Net
open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.UI

open Model
open Story
open Thumbnail

[<AutoOpen>]
module private Helpers = 

    type ClientStatistics =
        {
            session         : Guid
            name            : string
            frameCount      : int
            invalidateTime  : float
            renderTime      : float
            compressTime    : float
            frameTime       : float
        }

    let baseAddress = "http://localhost:4321"

    let size = V2i (256, 196)

    let controlId = lazy (
        use wc = new WebClient ()

        let rec get () =
            let result = wc.DownloadString (sprintf "%s/rendering/stats.json" baseAddress)
            let stats : ClientStatistics list =
                    result |> Pickler.unpickleOfJson

            match stats with
                | [] -> get ()
                | x::_ -> x.name

        get ()
    )

    // Creates a thumbnail by taking a screenshot
    let create () =
        use wc = new WebClient ()
        wc.DownloadData (sprintf "%s/rendering/screenshot/%s?w=%d&h=%d&samples=8" baseAddress controlId.Value size.X size.Y)
            |> ByteArray

// Empty thumbnail
let empty =
    { data = ByteArray Array.empty
      displaySize = size }

// Adds a thumbnail request
let request (id : SlideId) (model : Model) = 
    model |> Lens.update (Model.Lens.story |. Story.Lens.thumbnailRequests) (HSet.add id)

// Adds a blocking thumbnail request
let syncRequest (id : SlideId) (model : Model) =
    let f = Lens.set (Slide.Lens.thumbnail |. Thumbnail.Lens.data) <| create ()
    model |> Lens.update Model.Lens.story (Story.tryUpdateById id f)

let update (id : SlideId) (msg : ThumbnailAction) (model : Model) =
    match msg with
        | Updated d ->
            let f = Lens.set (Slide.Lens.thumbnail |. Thumbnail.Lens.data) d

            model |> Lens.update Model.Lens.story (Story.tryUpdateById id f)
                  |> Lens.update (Model.Lens.story |. Story.Lens.thumbnailRequests) (HSet.remove id)
        | Resized s ->
            let f = Lens.set (Slide.Lens.thumbnail |. Thumbnail.Lens.displaySize) s
            model |> Lens.update Model.Lens.story (Story.tryUpdateById id f)

let threads (model : Model) =

    let thumbnailProc (id : SlideId) =
        proclist {
            do! Async.SwitchToNewThread ()
            yield id, Updated <| create ()
        }

    model.story.thumbnailRequests
        |> HSet.fold (fun p id ->
            p |> ThreadPool.add (string id) (thumbnailProc id)
        ) ThreadPool.empty
