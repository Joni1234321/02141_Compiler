module FMPredicate

type e =
    | Constant      of int
    | LogicalVar    of string
    | ProgramVar    of string
    | PlusExpr      of (e * e)
    | MinusExpr     of (e * e)
    | TimesExpr     of (e * e)
    | ModExpr       of (e * e)
    | DivExpr       of (e * e)
    | UPlusExpr     of (e)
    | UMinusExpr    of (e)
    | PowExpr       of (e * e)
    | MathFunction  of (string * e list)


// 0 <= i
// i < j
// j < n
// ForAllPred (i,j) (IfPred(And[GEPred (0, i), GPred(i, j), GPred(j, n)]), 0 <= CustomPred(pi, []))


type Predicate =
    | BoolPred          of bool
    | AndPred           of (Predicate * Predicate)
    | OrPred            of (Predicate * Predicate)
    | NotPred           of Predicate
    | IfPred            of (Predicate * Predicate)
    | ExistsPred        of (string list * Predicate)
    | ForAllPred        of (string list * Predicate)
    | EqualPred         of (e * e)
    | NEqualPred        of (e * e)
    | GreaterPred       of (e * e)
    | GreaterEqualPred  of (e * e)
    | LessPred          of (e * e)
    | LessEqualPred     of (e * e)
    | CustomPred        of (string * e list)


let partialPredicateFactorial = Map [
        (0, AndPred(
                EqualPred(ProgramVar("x"), LogicalVar("n")), 
                GreaterEqualPred(LogicalVar("n"), Constant(0))));
        (1, AndPred(
                GreaterEqualPred(LogicalVar("n"), ProgramVar("x")),
                AndPred(
                    GreaterEqualPred(ProgramVar("x"), Constant(0)),
                    EqualPred(
                        TimesExpr(ProgramVar("y"), MathFunction("fac", [ProgramVar("x")])), 
                        MathFunction("fac", [LogicalVar("n")])))));
        (-1, EqualPred(ProgramVar("y"), MathFunction("fac", [LogicalVar("n")])))]

type Fisk = (int * Alpha list * int)
// Returns set of computed shortest paths
let computeShortestPath (pg : ProgramGraph) (preds : Map<int, Predicate>) : Set<Fisk> = 
    let rec build ((q0, w, q1) : Fisk) : Set<Fisk> =
        // Get all edges that go to q0
        let toQ1 = List.filter (fun (_, _, qend) -> if qend = q0 then true else false) pg

        List.foldBack (fun (q, alpha, qstart) s -> 
            // if the partial predicate contains qstart then add this to the set of short path fragments
            if Map.containsKey q preds then Set.union s (Set.empty.Add(q, alpha::w, q1))
            // else build the next
            else build (q, (alpha::w), q1)) toQ1 Set.empty
    
    // Do for all 
    Map.foldBack (fun q p s -> Set.union s (build (q, [], q))) preds Set.empty  

// let evalExpression (programMem, logicalMem) expression = 
//     let evalE e = evalExpression (programMem, logicalMem) e
//     match expression with 
//     | 
// let evalPredicate (programMem, logicalMem) predicate : bool = 
//     let evalP p = evalPredicate (programMem, logicalMem) p
//     match predicate with
//     | BoolPred  (b) -> b
//     | AndPred   (p1, p2)    -> (evalP p1) && (evalP p2)
//     | OrPred    (p1, p2)    -> (evalP p1) || (evalP p2)
//     | NotPred   (p0)        -> not (evalP p0)
//     | IfPred    (p1, p2)    ->  if evalP p1 then evalP p2
//                                 else true 
//     | ExistsPred (ls, p0)   ->  
//     | ForAllPred (ls, p0)   ->   
//     | EqualPred (e0, e1)    -> 
//     | NEqualPred        of (e * e)
//     | GreaterPred       of (e * e)
//     | GreaterEqualPred  of (e * e)
//     | LessPred          of (e * e)
//     | LessEqualPred     of (e * e)
//     | CustomPred