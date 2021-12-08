module Program

open Inferrer

[<EntryPoint>]
let main argv =
  argv 
  |> Seq.map (fun arg -> 
    async {
      let compilerType = inferCompiler arg 
      printfn "%s: %A" arg compilerType
    }
  ) 
  |> Async.Parallel
  |> Async.Ignore
  |> Async.RunSynchronously
  0