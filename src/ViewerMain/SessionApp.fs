module SessionApp

open Aardvark.Base
open Aardvark.UI
open Aardvark.Service

open Model
open Session
open Provenance
open Provenance.Reduced
open Story

[<AutoOpen>]
module private Helpers =
    open System
    open System.IO
    open MBrace.FsPickler
    open OpcSelectionViewer
    open Aardvark.UI.Primitives

    type SessionAppModel = {
        camera : CameraView
    }

    type SessionModel = {
        appModel : SessionAppModel
        dockConfig : DockConfig
        provenance : Provenance
        story : Story
        renderControlSize : V2i
        directory : string
    }

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module SessionAppModel =
    
        let create (model : AppModel) = {
            camera = AppModel.getCamera model
        }

        let restore (current : AppModel) (model : SessionAppModel) = {
            current with camera = { current.camera with view = CameraView.restore model.camera }
        }      

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module SessionModel =

        let create (model : Model) = {
            appModel = SessionAppModel.create model.appModel
            dockConfig = model.dockConfig
            provenance = model.provenance
            story = model.story
            renderControlSize = model.renderControlSize
            directory = model.directory
        }

        let restore (current : Model) (model : SessionModel) =
            let m = {
                current with appModel = SessionAppModel.restore current.appModel model.appModel
                             dockConfig = model.dockConfig
                             provenance = model.provenance
                             story = model.story
                             renderControlSize = model.renderControlSize
                             directory = model.directory
            }

            m |> Lens.set Model.Lens.appModel (State.restore m.appModel <| Provenance.state m.provenance)

    // XML serializer
    let private xmlSerializer =
        FsPickler.CreateXmlSerializer ()

    let pickle (model : Model) =
        let makeRelative (dir : string) =
            Path.GetRelativePath (Environment.CurrentDirectory, dir)

        model |> Lens.update (Model.Lens.directory) makeRelative
              |> SessionModel.create
              |> xmlSerializer.PickleToString

    let unpickle (init : string -> Model) (data : string) : Model =
        let s : SessionModel = xmlSerializer.UnPickleOfString data
        let m = init s.directory
        
        SessionModel.restore m s

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

let update (msg : SessionAction) (init : string -> Model) (model : Model) =
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
                let model = file |> File.readAllText |> unpickle init
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