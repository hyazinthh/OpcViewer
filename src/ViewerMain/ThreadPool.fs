namespace Aardvark.Base.Incremental

[<AutoOpen>]
module ThreadPoolExtensions =
    module ThreadPool =

        let single (id : string) (proc : ProcList<'a, unit>) =
            ThreadPool.empty |> ThreadPool.add id proc

        let unionMany (threads : ThreadPool<'a> list) = 
            List.fold ThreadPool.union ThreadPool.empty threads

