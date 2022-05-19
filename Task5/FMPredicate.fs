module FMPredicate

type e =
    | Constant      of int
    | LogicalVar    of string
    | ProgramVar    of string
    | PlusExprE      of (e * e)
    | MinusExprE     of (e * e)
    | TimesExprE     of (e * e)
    | ModExprE       of (e * e)
    | DivExprE       of (e * e)
    | UPlusExprE     of (e)
    | UMinusExprE    of (e)
    | PowExprE       of (e * e)
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
                        TimesExprE(ProgramVar("y"), MathFunction("fac", [ProgramVar("x")])), 
                        MathFunction("fac", [LogicalVar("n")])))));
        (-1, EqualPred(ProgramVar("y"), MathFunction("fac", [LogicalVar("n")])))]

type SPF = (int * Alpha list * int)
// Returns set of computed shortest paths
let computeShortestPath (pg : ProgramGraph) (preds : Map<int, Predicate>) : Set<SPF> = 
    let rec build ((q0, w, q1) : SPF) : Set<SPF> =
        // Get all edges that go to q0
        let toQ1 = List.filter (fun (_, _, qend) -> if qend = q0 then true else false) pg

        List.foldBack (fun (q, alpha, qstart) s -> 
            // if the partial predicate contains qstart then add this to the set of short path fragments
            if Map.containsKey q preds then Set.union s (Set.empty.Add(q, alpha::w, q1))
            // else build the next
            else build (q, (alpha::w), q1)) toQ1 Set.empty
    
    // Do for all 
    Map.foldBack (fun q p s -> Set.union s (build (q, [], q))) preds Set.empty

// Converts type of a to type e
let rec convertAtoE = function 
    | Num (n) -> Constant(int(n))
    | Var (s) -> ProgramVar(s)
    | ArrayVar (s, a) -> ProgramVar(sprintf "%s %A" s (convertAtoE a))  // THIS HERE DOES NOT WORK YET
    | PlusExpr (a1, a2) -> PlusExprE(convertAtoE a1, convertAtoE a2)
    | MinusExpr (a1, a2) -> MinusExprE(convertAtoE a1, convertAtoE a2)
    | TimesExpr (a1, a2) -> TimesExprE(convertAtoE a1, convertAtoE a2)
    | ModExpr (a1, a2) -> ModExprE(convertAtoE a1, convertAtoE a2)
    | DivExpr (a1, a2) -> DivExprE(convertAtoE a1, convertAtoE a2)
    | UPlusExpr (a) -> UPlusExprE(convertAtoE a)
    | UMinusExpr (a) -> UMinusExprE(convertAtoE a)
    | PowExpr (a1, a2) -> PowExprE(convertAtoE a1, convertAtoE a2)

// replace the var in the expression with the value
let rec replaceExpression var value expression = 
    // Higher order with fewer arguments
    let replaceE e = replaceExpression var value e
    match expression with 
    | Constant (n) -> expression
    | LogicalVar (s) -> expression
    | ProgramVar (s) -> 
        // if it is the correct variable
        if s = var then convertAtoE value
        else expression
    | PlusExprE (e1, e2) -> PlusExprE(replaceE e1, replaceE e2)
    | MinusExprE (e1, e2) -> MinusExprE(replaceE e1, replaceE e2)
    | TimesExprE (e1, e2) -> TimesExprE(replaceE e1, replaceE e2)
    | ModExprE  (e1, e2) -> ModExprE(replaceE e1, replaceE e2)
    | DivExprE  (e1, e2) -> DivExprE(replaceE e1, replaceE e2)
    | UPlusExprE (e) -> UPlusExprE(replaceE e)
    | UMinusExprE (e) -> UMinusExprE(replaceE e)
    | PowExprE   (e1, e2) -> PowExprE(replaceE e1, replaceE e2)
    | MathFunction (s, e) -> MathFunction (s, List.foldBack (fun e acc -> (replaceE e)::acc) e [])

// Replace the var in the predicate
let rec replacePredicate (var : string) (value : a) (predicate : Predicate) : Predicate =
    // higher order function so you dont have to type all parameters
    let replaceE e = replaceExpression var value e
    let replaceP p = replacePredicate var value p
    match predicate with 
    | BoolPred (b)  -> predicate
    | AndPred (p1, p2) -> AndPred(replaceP p1, replaceP p2)
    | OrPred (p1, p2) -> OrPred(replaceP p1, replaceP p2)
    | NotPred (p) -> NotPred(replaceP p)
    | IfPred (p1, p2) -> IfPred(replaceP p1, replaceP p2)
    | ExistsPred (s, p) -> ExistsPred(s, replaceP p)
    | ForAllPred (s, p) -> ForAllPred(s, replaceP p)
    | EqualPred (e1, e2) -> EqualPred(replaceE e1, replaceE e2)
    | NEqualPred (e1, e2) -> NEqualPred(replaceE e1, replaceE e2)
    | GreaterPred (e1, e2) -> GreaterPred(replaceE e1, replaceE e2)
    | GreaterEqualPred (e1, e2) -> GreaterEqualPred(replaceE e1, replaceE e2)
    | LessPred (e1, e2) -> LessPred(replaceE e1, replaceE e2)
    | LessEqualPred (e1, e2) -> LessEqualPred(replaceE e1, replaceE e2)
    | CustomPred (s, e) -> CustomPred(s, List.foldBack (fun e acc -> (replaceE e)::acc) e [])


let reduceSPF (spf : Set<SPF>) (preds : Map<int, Predicate>) : (int * Predicate * int) list =
    let reduce (alpha : Alpha list) (predicate : Predicate) : Predicate = 
        // For every alpha in the spf reduce the predicate
        List.foldBack (fun alpha pred -> 
            match alpha with
            | AlphaC (AssignStatement(s, a)) -> replacePredicate s a pred
            // TODO: Implement array verification
            | _ -> pred) alpha predicate
    // Foreach SPF in the set reduce it
    Set.foldBack (fun spf out ->
        let (q0, alpha, q1) = spf 
        (q0, reduce alpha (Map.find q0 preds), q1)::out) spf []


let rec printExpression expr = 
    match expr with 
    | Constant (n) -> sprintf "%d" n
    | ProgramVar (s) -> s
    | PlusExprE (e1, e2) -> sprintf "(%s + %s)" (printExpression e1) (printExpression e2)
    | MinusExprE (e1, e2) -> sprintf "(%s - %s)" (printExpression e1) (printExpression e2)
    | TimesExprE (e1, e2) -> sprintf "(%s * %s)" (printExpression e1) (printExpression e2)
    | ModExprE  (e1, e2) -> sprintf "(%s %% %s)" (printExpression e1) (printExpression e2)
    | DivExprE  (e1, e2) -> sprintf "(%s / %s)" (printExpression e1) (printExpression e2)
    | UPlusExprE (e) -> sprintf "(+%s)" (printExpression e)
    | UMinusExprE (e) -> sprintf "(-%s)" (printExpression e)
    | PowExprE   (e1, e2) -> sprintf "(%s ^ (%s))" (printExpression e1) (printExpression e2)
    | MathFunction (s, e) -> sprintf "MathFunction(%s, [%s])" s (List.foldBack (fun e s -> sprintf "%s, %s" (printExpression e) s) e "")
    | _ -> sprintf "%A" expr

let rec printPredicate pred =
    match pred with
    | BoolPred (b)  -> sprintf "%b" b
    | AndPred (p1, p2) -> sprintf "(%s & %s)" (printPredicate p1) (printPredicate p2)
    | OrPred (p1, p2) -> sprintf "(%s | %s)" (printPredicate p1) (printPredicate p2)
    | NotPred (p) -> sprintf "!%s" (printPredicate p)
    | IfPred (p1, p2) -> sprintf "(%s ⇒ %s)" (printPredicate p1) (printPredicate p2)
    | ExistsPred (s, p) -> sprintf "((E%A) %s)" s (printPredicate p)
    | ForAllPred (s, p) -> sprintf "((A%A) %s)" s (printPredicate p)
    | EqualPred (e1, e2) -> sprintf "(%s == %s)" (printExpression e1) (printExpression e2)
    | NEqualPred (e1, e2) -> sprintf "(%s != %s)" (printExpression e1) (printExpression e2)
    | GreaterPred (e1, e2) -> sprintf "(%s > %s)" (printExpression e1) (printExpression e2)
    | GreaterEqualPred (e1, e2) -> sprintf "(%s >= %s)" (printExpression e1) (printExpression e2)
    | LessPred (e1, e2) -> sprintf "(%s < %s)" (printExpression e1) (printExpression e2)
    | LessEqualPred (e1, e2) -> sprintf "(%s <= %s)" (printExpression e1) (printExpression e2)
    | _ -> sprintf "%A" pred
    // | CustomPred (s, e) -> CustomPred(s, List.foldBack (fun e acc -> (replaceE e)::acc) e [])
