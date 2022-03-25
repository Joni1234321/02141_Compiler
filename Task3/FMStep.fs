module FMStep

type Step =
    | StepB of (b * int) list
    | StepC of (c * int)

type ProgramMap = Map<int, Step>
let emptyProgramMap = Map.empty<int, Step>

// Convert From Graph to Map
let getMap (pg : ProgramGraph) : ProgramMap =
    List.foldBack (fun edge (programMap : ProgramMap) -> 
        // Extract key and step from program graph
        match edge with 
        | EdgeB(q0, bexpr, q1) -> 
            // Append to list 
            match Map.tryFind q0 programMap with 
            | Some(StepB(ls)) -> Map.add q0 (StepB(ls @ [bexpr, q1])) programMap
            | None -> Map.add q0 (StepB[bexpr, q1]) programMap
            
            | _ ->
                // In case of [] or already existsing StepC at q  
                printfn "Fail, Cannot convert to Map since q is stepc or [], while it should be stepb"
                programMap
        
        // Can only exists one of this at the q
        | EdgeC(q0, cexpr, q1) -> 
            Map.add q0 (StepC(cexpr, q1)) programMap) pg emptyProgramMap




// Give state, the programMap and memory and return memory and next state
let evalStep (q : int) (pm : ProgramMap) (mem : Map<string, VType>) : (int * Map<string, VType>) =
    printfn "q%i - %A" q mem
    match Map.tryFind q pm with 
    | Some(StepB(ls)) ->
        // If no condition is true then it raises an exception 
        let (_, q1) = List.findBack (fun (b, q1) -> evalb b mem) ls 
        (q1, mem)
    | Some(StepC(c, q1)) -> (q1, evalc c mem)
    | None ->
        // TODO: FAIL HAS REACHED STATE THAT DOESNT EXIST 
        printfn "ERROR THIS SHOULD NOT BE REACHED"
        (-1, mem)


// Run program n times
let rec evalN n q pg mem = 
    match n with 
    | n when q >= 0 ->
        let (q1, mem1) = evalStep q pg mem
        evalN (n-1) q1 pg mem1
    | _ -> 
        (q, mem)