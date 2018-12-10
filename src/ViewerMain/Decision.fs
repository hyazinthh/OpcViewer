namespace Provenance

type 'a Decision =
    | Decided of 'a
    | Undecided of 'a

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Decision =
    let map f = function
        | Undecided x -> f x
        | x -> x

    let get = function
        | Decided x | Undecided x -> x
