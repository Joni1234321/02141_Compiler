module FMGraph


//let evalGC 
let rec doneGC gc =
    match gc with
    | ThenStatement (b, c) -> NotOp b
    | ElseStatement (gc1, gc2) -> AndOp (doneGC gc1, doneGC gc2)

let rec doneGCd gc = 
    match gc with
    | ThenStatement (b, c) -> NotOp b
    | ElseStatement (gc1, gc2) -> AndOp (doneGC gc1, doneGC gc2)
    

let rec graphA a : string = 
    match a with
    | Num(n) -> sprintf "%f" n
    | Var(x) -> x
    | ArrayVar(x, a) -> sprintf "%s[%s]" x (graphA a)
    | PlusExpr (a1, a2) -> sprintf "%s + %s" (graphA a1) (graphA a2)
    | MinusExpr (a1, a2) -> sprintf "%s - %s" (graphA a1) (graphA a2)
    | TimesExpr (a1, a2) -> sprintf "%s * %s" (graphA a1) (graphA a2)
    | ModExpr (a1, a2) -> sprintf "%s %% %s" (graphA a1) (graphA a2)
    | DivExpr (a1, a2) -> sprintf "%s / %s" (graphA a1) (graphA a2)
    | UPlusExpr (a) -> sprintf "+%s" (graphA a)
    | UMinusExpr (a) -> sprintf "-%s" (graphA a)
    | PowExpr (a1, a2) -> sprintf "%s ^ %s" (graphA a1) (graphA a2)

let rec graphB b : string = 
    match b with
    | Bool(x) -> sprintf "%b" x
    | AndOp(b1, b2) -> sprintf "( %s ) & ( %s )" (graphB b1) (graphB b2)
    | OrOp(b1, b2) -> sprintf "( %s ) | ( %s )" (graphB b1) (graphB b2)
    | ForAllOp(b1, b2) -> sprintf "( %s ) && ( %s )" (graphB b1) (graphB b2)
    | ExistsOp (b1, b2) -> sprintf "( %s ) || ( %s )" (graphB b1) (graphB b2)
    | NotOp (b) -> sprintf "!( %s )" (graphB b) 
    | EqualOp (a1, a2) -> sprintf "( %s ) = ( %s )" (graphA a1) (graphA a2)
    | NEqualOp (a1, a2) -> sprintf "( %s ) != ( %s )" (graphA a1) (graphA a2)
    | GreaterOp (a1, a2) -> sprintf "( %s ) > ( %s )" (graphA a1) (graphA a2)
    | GreaterEqualOp (a1, a2) -> sprintf "( %s ) >= ( %s )" (graphA a1) (graphA a2)
    | LessOp (a1, a2) -> sprintf "( %s ) < ( %s )" (graphA a1) (graphA a2)
    | LessEqualOp (a1, a2) -> sprintf "( %s ) <= ( %s )" (graphA a1) (graphA a2)

type Edge = (int * string * int)
type ProgramGraph = Edge list

let graph c (deterministic : bool) : ProgramGraph = 
    let mutable q = 0
    
    // Non deterministic
    let rec graphC (q0 : int) (q1 : int) c : ProgramGraph =
        match c with 
        | MultipleStatement(c1, c2) -> 
            q <- q + 1
            let _q = q
            let e1 = graphC q0 _q c1
            let e2 = graphC _q q1 c2
            e1 @ e2
        | IfStatement(gc) -> 
            graphGC q0 q1 gc
        | DoStatement(gc) -> 
            let b = doneGC gc
            let e = graphGC q0 q0 gc
            e @ [(q0, graphB b, q1)]
            
        | AssignStatement(x, a) ->
            let s = sprintf "%s:=%s" x (graphA a)
            [(q0, s, q1)]
        | AssignArrayStatement(x, i, a) ->
            let s = sprintf "%s[%s]:=%s" x (graphA i) (graphA a)
            [(q0, s, q1)]
        | SkipStatement -> 
            [(q0, "skip", q1)]

    and graphGC (q0 : int) (q1 : int) gc : ProgramGraph = 
        match gc with
        | ThenStatement(b, c) ->
            q <- q + 1
            let _q = q
            let s = graphB b
            let e = graphC _q q1 c
            [(q0, s, _q)] @ e
        | ElseStatement(gc1, gc2) -> 
            let e1 = graphGC q0 q1 gc1
            let e2 = graphGC q0 q1 gc2
            e1 @ e2

    // Deterministic
    let rec graphCd (q0 : int) (q1 : int) c : ProgramGraph =
        match c with 
        | MultipleStatement(c1, c2) -> 
            q <- q + 1
            let _q = q
            let e1 = graphCd q0 _q c1
            let e2 = graphCd _q q1 c2
            e1 @ e2
        | IfStatement (gc) ->
            let (e, _) = graphGCd q0 q1 gc (Bool false)
            e
        | DoStatement (gc) ->
            let b = doneGC gc
            let (e, d) = graphGCd q0 q0 gc (Bool false)
            e @ [(q0, graphB (NotOp d), q1)] 
        | _ -> graphC q0 q1 c
    and graphGCd (q0 : int)  (q1 : int) gc (d : b) : (ProgramGraph * b) =
        match gc with 
        | ThenStatement (b, c) -> 
            q <- q + 1
            let _q = q
            let s = graphB (AndOp (b, NotOp d))
            let e = graphCd _q q1 c
            ([(q0, s, _q)] @ e, OrOp(b, d))
        | ElseStatement (gc1, gc2) ->
            let (e1, d1) = graphGCd q0 q1 gc1 d
            let (e2, d2) = graphGCd q0 q1 gc2 d1
            (e1 @ e2, d2)

    let start = 0
    let final = 1000000

    if deterministic then graphCd start final c 
    else graphC start final c
