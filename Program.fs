module Program

open Inferrer

let numCores = System.Environment.ProcessorCount - 2

[<EntryPoint>]
let main argv =
  argv 
  |> Seq.chunkBySize numCores |> Seq.toArray
  |> Array.iter (fun args ->
    args |> Array.Parallel.iter (fun binPath -> 
      fprintfn stderr "Analyzing %s" binPath 
      let compilerType = inferCompiler binPath 
      printfn "%s: %A" binPath compilerType
    )
  )
  0