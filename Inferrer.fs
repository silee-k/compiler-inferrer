module Inferrer

open B2R2
open B2R2.FrontEnd
open B2R2.BinIR.LowUIR
open B2R2.FrontEnd.BinLifter.Intel
open B2R2.FrontEnd.BinInterface
open B2R2.MiddleEnd.BinEssence
open B2R2.MiddleEnd.BinGraph
open B2R2.MiddleEnd.ControlFlowGraph
open B2R2.MiddleEnd.ControlFlowAnalysis

type RegType =
  | General
  | Index
  | Stack
  | Extra
  | Others

type OrderType = 
  | Asc
  | Desc
  | Unknown

type PushInfo = 
  { 
    Reg: Register
    Type: RegType
    Order: OrderType
  }

type CompilerType =
  | Gcc = 0
  | Clang = 1
  | Icc = 2

type Result = 
  { 
    Gcc: int
    Clang: int
    Icc: int
  }

let getRegisterType reg =
  match Register.extendRegister64 reg with
  | Register.RAX | Register.RCX | Register.RDX | Register.RBX  
    -> RegType.General
  | Register.RSI | Register.RDI -> RegType.Index
  | Register.RSP | Register.RBP -> RegType.Stack
  | Register.R8 | Register.R9 | Register.R10 | Register.R11 
  | Register.R12 | Register.R13 | Register.R14 | Register.R15 
    -> RegType.Extra 
  | _ -> RegType.Others

let compilerTypes = [| CompilerType.Gcc; CompilerType.Clang; CompilerType.Icc |]
  
let inferCompiler (binPath: string) =
  let stopwatch = System.Diagnostics.Stopwatch();
  stopwatch.Start();
  let hdl = BinHandle.Init(ISA.DefaultISA, binPath)
  let ess = BinEssence.init hdl [] [] []
  let funcs = ess.CodeManager.FunctionMaintainer.RegularFunctions
  stopwatch.Stop();
  printfn "B2R2 Time: %d" stopwatch.ElapsedMilliseconds
  fprintfn stderr "MiddleEnd Analyzed: %s" binPath
  stopwatch.Restart();
  let getPushInfo (func: RegularFunction) =
    let unreachables = DiGraph.getUnreachables func.IRCFG 
    let entryVertex = 
      unreachables |> List.tryFind (fun v -> v.VData.PPoint.Address = func.Entry)
    match entryVertex with
    | None -> None
    | Some entryVertex -> 
      let insInfos = entryVertex.VData.InsInfos
      let pushes = 
        insInfos |> Array.fold (fun (pushes, breaked) info -> 
          let decomposed = info.Instruction.Decompose (false)
          let mnemonic = decomposed[0].AsmWordValue
          let op1 = Array.tryItem 1 decomposed
          match mnemonic with
          | "push" when not breaked -> info :: pushes, breaked
          | "sub" when (Option.get op1).AsmWordValue.EndsWith("sp") -> 
            pushes, true
          | _ -> pushes, breaked
        ) ([], false) |> fst |> List.rev

      let regIds = 
        pushes |> List.fold (fun regIds push -> 
          push.Stmts |> Array.fold (fun regIds stmt ->
            match stmt.S with
            | Put (_, e2) -> 
              match e2.E with
              | Var (_, id, _, _ ) -> id :: regIds
              | _ -> regIds
            | _ -> regIds
          ) regIds
        ) [] |> List.rev

      let regs = regIds |> List.map (fun id -> Register.ofRegID id) 

      let pushInfos = 
        regs |> Array.ofList |> Array.fold (fun pushInfos reg ->
          let info = { Reg = reg; Type = getRegisterType reg; Order = Unknown}
          if info.Type <> RegType.Others then  info :: pushInfos
          else pushInfos
        ) [] |> List.rev |> List.toArray

      let pushInfos = 
        pushInfos |> Array.foldi (fun infos i info ->
          let info = 
            if i + 1 < Array.length pushInfos then
              let nextInfo = pushInfos[i + 1]
              let id1, id2 = 
                Register.toRegID info.Reg, Register.toRegID nextInfo.Reg
              if info.Type = nextInfo.Type then
                if id1 < id2 then { info with Order = Asc }
                else { info with Order = Desc }
              else 
                if i > 0 && info.Type = pushInfos[i - 1].Type then
                  // the order of last in the same type is previous one
                  { info with Order = (List.head infos).Order }
                else info
            else // the order of last in the same type is previous one
              if i > 0 && info.Type = pushInfos[i - 1].Type then
                { info with Order = (List.head infos).Order }
              else info
          info :: infos
        ) [] |> fst |> List.rev

      let uniquePushInfos = 
        pushInfos |> List.fold (fun acc info ->
          let exist = acc |> List.exists (fun setInfo -> 
                      setInfo.Type = info.Type && setInfo.Order = info.Order)
          if exist then acc else info :: acc
        ) [] |> List.rev 

      if uniquePushInfos.Length > 1 then Some (func.Entry, uniquePushInfos)
      else None

  let funcPushInfos = funcs |> Array.map getPushInfo |> Array.choose id

  let gccPushOrder = [| RegType.Extra; RegType.Stack; RegType.Index;  RegType.General |]
  let clangPushOrder = [| RegType.Stack; RegType.Extra; RegType.General; RegType.Index |]
  let iccPushOrder = [| RegType.Extra; RegType.General; RegType.Stack |]
  let orders = [ gccPushOrder; clangPushOrder; iccPushOrder ];

  let comparer order i1 i2 = 
    let rank1 = order |> Array.tryFindIndex i1.Type.Equals
    let rank2 = order |> Array.tryFindIndex i2.Type.Equals
    match rank1, rank2 with
    | Some rank1, Some rank2 -> compare rank1 rank2
    | None, _ | _, None | None, None -> 0

  let matchPushOrder compilerType = 
    funcPushInfos 
    |> Array.map (fun (funcAddr, pushInfos) ->
      let compilerPushOrder = orders[int compilerType]
      let sortedPushInfos = 
        pushInfos |> List.sortWith (comparer compilerPushOrder)
      let matched = pushInfos = sortedPushInfos
      // fprintfn stderr "func %x: " funcAddr
      // fprintfn stderr "%b -> " matched
      // pushInfos |> List.iter (fun info ->
      //   fprintfn stderr "%A:%A " info.Type info.Order
      // )
      // fprintfn stderr " VS "
      // sortedPushInfos |> List.iter (fun info ->
      //   fprintfn stderr "%A:%A " info.Type info.Order
      // )
      // fprintfn stderr "\n" 
      matched
    ) 
  let scores = 
    compilerTypes 
    |> Array.map (fun compilerType ->
      let matches = matchPushOrder compilerType
      let numMatches = matches |> Seq.sumBy System.Convert.ToInt32
      numMatches
    ) 

  let probabilities = 
    scores
    |> Array.mapi (fun i matches -> float matches / float funcPushInfos.Length)
  
  let result = 
    compilerTypes
    |> Array.mapi (fun i compiler -> (compiler, scores[i], probabilities[i]))
  let sortedResult = result |> Array.sortByDescending (fun (_, s, _) -> s)
  stopwatch.Stop();
  printfn "Inferring Time: %d" stopwatch.ElapsedMilliseconds
  sortedResult
