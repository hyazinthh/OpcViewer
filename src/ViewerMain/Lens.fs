namespace Aardvark.Base

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Lens = 

    let get (lens : Lens<'s, 'a>) (s : 's) : 'a = 
        lens.Get (s)
        
    let set (lens : Lens<'s, 'a>) (v : 'a) (s : 's) : 's =
        lens.Set (s, v)
        
    let set' (lens : Lens<'s, 'a>) (s : 's) (v : 'a) : 's =
        lens.Set (s, v)
    
    let update (lens : Lens<'s, 'a>) (f : 'a -> 'a) (s : 's) : 's =
        lens.Update (s, f)
