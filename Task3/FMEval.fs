module FMEval

open FM4FUNTypesAST
open FMMemory

// The mem doen't change when it goes through the a or b expressions
// a -> mem -> float
let rec evala expression mem =
    match expression with 
    | Num(n) -> n
    | Var(x) -> 
        match Map.find x mem with 
            | Variable(f) -> f
            | ArrayVariable(_) -> 42   // TODO: Fail
    | ArrayVar(x, a) -> 
        match Map.find x mem with 
            | Variable(_) -> 42     // TODO: Fail 
            | ArrayVariable(array) -> Map.find (int (evala a mem)) array
    | PlusExpr (a1, a2) -> (evala a1 mem) + (evala a2 mem)
    | MinusExpr (a1, a2) -> (evala a1 mem) - (evala a2 mem)
    | TimesExpr (a1, a2) -> (evala a1 mem) * (evala a2 mem)
    | ModExpr (a1, a2) -> (evala a1 mem) % (evala a2 mem)
    | DivExpr (a1, a2) -> (evala a1 mem) / (evala a2 mem)
    | UPlusExpr (a) -> (evala a mem)
    | UMinusExpr (a) -> - (evala a mem)
    | PowExpr (a1, a2) -> (evala a1 mem) ** (evala a2 mem)

// b -> mem -> bool
let rec evalb operator mem = 
    match operator with
    | Bool(x) -> x
    | AndOp(b1, b2) -> 
        let (eb1, eb2) = ((evalb b1 mem), (evalb b2 mem))
        eb1 && eb2
    | OrOp(b1, b2) -> 
        let (eb1, yb2) = ((evalb b1 mem), (evalb b2 mem))
        eb1 || yb2
    | ForAllOp(b1, b2) -> (evalb b1 mem) && (evalb b2 mem)
    | ExistsOp (b1, b2) -> (evalb b1 mem) || (evalb b2 mem)
    | NotOp (b) -> not (evalb b mem)
    | EqualOp (a1, a2) -> (evala a1 mem) = (evala a2 mem)
    | NEqualOp (a1, a2) -> (evala a1 mem) <> (evala a2 mem)
    | GreaterOp (a1, a2) -> (evala a1 mem) > (evala a2 mem)
    | GreaterEqualOp (a1, a2) -> (evala a1 mem) >= (evala a2 mem)
    | LessOp (a1, a2) -> (evala a1 mem) < (evala a2 mem)
    | LessEqualOp (a1, a2) -> (evala a1 mem) <= (evala a2 mem)

// c -> mem -> mem
let rec evalc statement mem =
    match statement with
    | AssignStatement(x, a) -> 
        let value = evala a mem

        match Map.tryFind x mem with 
            | Some(v) -> 
                match v with
                    | Variable(_) -> Map.add x (Variable(value)) mem
                    | ArrayVariable(_) -> mem   // TODO: Fail
            | None -> Map.add x (Variable(value)) mem
    | AssignArrayStatement(x, i, a) ->  
        let index = int(evala i mem)
        let value = evala a mem

        match Map.tryFind x mem with 
            | Some(v) -> 
                match v with
                    | Variable(_) -> mem            // TODO: Fail
                    | ArrayVariable(array) -> 
                        let newarray = ArrayVariable(Map.add index value array)
                        Map.add x newarray mem
            | None -> 
                let newarray = ArrayVariable(Map[(index, value)])
                Map.add x newarray mem
    | SkipStatement -> mem
    | _ ->
        printfn "Should be unreachable" 
        mem
