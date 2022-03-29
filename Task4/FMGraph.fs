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
    

type Alpha = 
    | AlphaB of b
    | AlphaC of c

type ProgramGraph = (int * Alpha * int) list

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
        | IfStatement(gc) ->                graphGC q0 q1 gc
        | DoStatement(gc) -> 
            let b = doneGC gc
            let e = graphGC q0 q0 gc
            e @ [(q0, AlphaB(b), q1)]
        | AssignStatement(x, a) ->          [(q0, AlphaC(c), q1)]
        | AssignArrayStatement(x, i, a) ->  [(q0, AlphaC(c), q1)]
        | SkipStatement ->                  [(q0, AlphaC(c), q1)]

    and graphGC (q0 : int) (q1 : int) gc : ProgramGraph = 
        match gc with
        | ThenStatement(b, c) ->
            q <- q + 1
            let _q = q
            let e = graphC _q q1 c
            [(q0, AlphaB(b), _q)] @ e
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
            e @ [(q0, AlphaB(NotOp d), q1)] 
        | _ -> graphC q0 q1 c
    and graphGCd (q0 : int)  (q1 : int) gc (d : b) : (ProgramGraph * b) =
        match gc with 
        | ThenStatement (b, c) -> 
            q <- q + 1
            let _q = q
            let e = graphCd _q q1 c
            ([(q0, AlphaB(AndOp (b, NotOp d)), _q)] @ e, OrOp(b, d))
        | ElseStatement (gc1, gc2) ->
            let (e1, d1) = graphGCd q0 q1 gc1 d
            let (e2, d2) = graphGCd q0 q1 gc2 d1
            (e1 @ e2, d2)

    let start = 0
    let final = -1

    if deterministic then graphCd start final c 
    else graphC start final c
