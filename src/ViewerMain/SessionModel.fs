namespace Session

type SessionAction =
    | Load of string option
    | Save of string option
