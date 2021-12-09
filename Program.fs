module Program

open Inferrer

let numCores = 12

[<EntryPoint>]
let main argv =
  argv 
  |> Array.splitInto numCores
  |> Array.iter (fun args ->
    args |> Array.Parallel.iter (fun binPath -> 
      fprintfn stderr "Analyzing %s" binPath 
      let compilerType = inferCompiler binPath 
      printfn "%s: %A" binPath compilerType
    )
  )
  0