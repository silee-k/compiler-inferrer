module Program

open Inferrer

[<EntryPoint>]
let main argv =
  argv 
  |> Array.Parallel.iter (fun arg -> 
      fprintfn stderr "Analyzing %s" arg 
      let compilerType = inferCompiler arg 
      printfn "%s: %A" arg compilerType
  )
  0