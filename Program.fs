module Program

open Inferrer

let numCores = System.Environment.ProcessorCount - 2 |> max 1

[<EntryPoint>]
let main argv =
  argv 
  |> Seq.chunkBySize numCores |> Seq.toArray
  |> Array.iter (fun args ->
    args |> Array.Parallel.iter (fun binPath -> 
      fprintfn stderr "Analyzing %s" binPath 
      let result = inferCompiler binPath 
      let bestFitCompiler, _, _ = result[0]
      let rank = result |> Array.map (fun (c, _, _) -> c)
      printfn "%s: %A, rank : %A, probabilities: %A" binPath bestFitCompiler rank
       (result |> Array.fold (fun a (_, _, x) -> a @ [sprintf "%.3f" x]) [])
    )
  )
  0