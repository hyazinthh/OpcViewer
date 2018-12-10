module SessionApp

open Aardvark.Base
open Aardvark.UI
open Aardvark.Service

open Model
open Session
open Provenance

#nowarn "8989"

[<AutoOpen>]
module private Helpers =
    open MBrace.FsPickler

    // XML serializer
    let private xmlSerializer =

        // CameraView contains a lazy field, need a custom pickler
        let cameraPickler (resolver : IPicklerResolver) =
            let p = resolver.Resolve<Reduced.CameraView> ()

            let writer (w : WriteState) (ns : CameraView) =
                p.Write w "view" <| Reduced.CameraView.create ns

            let reader (r : ReadState) =
                let v = p.Read r "view"
                Reduced.CameraView.restore v

            Pickler.FromPrimitives (reader, writer)

        let registry = new CustomPicklerRegistry ()
        do registry.RegisterFactory cameraPickler
        let cache = PicklerCache.FromCustomPicklerRegistry registry
    
        FsPickler.CreateXmlSerializer(picklerResolver = cache)

    let pickle (model : Model) =
        xmlSerializer.PickleToString model

    let unpickle (data : string) : Model =
        xmlSerializer.UnPickleOfString data

[<AutoOpen>]
module private Events =
    let onChooseFile (chosen : string option -> 'msg) =
        let cb xs =
            match xs with
                | x::[] when x <> null ->
                    x |> Pickler.unpickleOfJson |> List.map PathUtils.ofUnixStyle |> List.tryHead |> chosen
                | _ ->
                    chosen None
        onEvent "onchoose" [] cb

    let onSaveFile (chosen : string option -> 'msg) =
        let cb xs =
            match xs with
                | x::[] when x <> null ->
                    x |> Pickler.unpickleOfJson |> PathUtils.ofUnixStyle |> Some |> chosen
                | _ ->
                    chosen None
        onEvent "onsave" [] cb

let update (msg : SessionAction) (model : Model) =
    match msg with
        | Save (Some file) ->
            Log.startTimed "Saving session to '%s'... " file
            try
                File.writeAllText file <| pickle model
                Log.stop ()
            with
                | e -> Log.error "%A" e
            
            model

        | Load (Some file) ->
            Log.startTimed "Loading session from '%s'... " file
            try
                let model = file |> File.readAllText |> unpickle
                Log.stop ()
                model
            with
                | e -> Log.error "%A" e
                       model

        | _ ->
            model

let view =
    let dependencies = Html.semui @ [
        { kind = Stylesheet; name = "menuStyle"; url = "Menu.css" }
    ]

    let initPopup =
        "$('.session.item').popup({ hoverable : true, variation : 'basic' });"

    let closePopup =
        "$('.session.item').popup('hide');"

    let openDialog =
        "aardvark.processEvent('__ID__', 'onchoose', aardvark.dialog.showOpenDialog({" +
            "properties: ['openFile']," + 
            "filters: [{ name: 'Session files', extensions: ['xml'] }]" +
            "}));"

    let saveDialog =
        "aardvark.processEvent('__ID__', 'onsave', aardvark.dialog.showSaveDialog({" +
            "filters: [{ name: 'Session files', extensions: ['xml'] }]" +
            "}));"

    require dependencies (
        onBoot initPopup (
            div [clazz "ui main menu"] [
                a [clazz "session item"] [
                    text "Session"
                    i [clazz "dropdown icon"] []
                ]

                div [clazz "ui popup"] [
                    div [clazz "ui vertical menu"] [
                        a [
                            clazz "item"
                            onChooseFile Load
                            clientEvent "onclick" closePopup
                            clientEvent "onclick" openDialog
                        ] [
                            text "Load"
                            i [clazz "open folder icon"] []
                        ]

                        a [
                            clazz "item"
                            onSaveFile Save
                            clientEvent "onclick" closePopup
                            clientEvent "onclick" saveDialog
                        ] [
                            text "Save"
                            i [clazz "save icon"] []
                        ]
                    ]
                ]
            ]
        )
    )