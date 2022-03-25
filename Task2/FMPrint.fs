module FMPrint

let printPG (pg : ProgramGraph) : string = 
    let rec print (pg : ProgramGraph) : string =
        match pg with 
        | [] -> ""
        | edge :: tail -> 
            let (q0, expr, q1) = edge
            let s = sprintf "q%i -> q%i [label=\"%s\"]" q0 q1 ((sprintf "%A" expr).Replace("\"", ""))
            sprintf "%s\n%s" s (print tail)

    sprintf "digraph G {\n%s}\n" (print pg)