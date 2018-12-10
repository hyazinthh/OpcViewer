namespace Thumbnail

open System
open Aardvark.Base
open Aardvark.Base.Incremental

// Each slide has a preview thumbnail which
// is saved as a byte array and passed to Javascript as a base64 string
type ThumbnailData =
    ByteArray of byte [] with

    override x.ToString () =
        let (ByteArray d) = x in d |> Convert.ToBase64String

// Additionally we keep track of the display size of
// the thumbnail for viewport calculations
[<DomainType>]
type Thumbnail = {
    data : ThumbnailData
    displaySize : V2i
}

type ThumbnailAction =
    | Updated of ThumbnailData
    | Resized of V2i
