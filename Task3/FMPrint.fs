module FMPrint


let rec printA a : string = 
    match a with
    | Num(n) -> sprintf "%g" n
    | Var(x) -> x
    | ArrayVar(x, a) -> sprintf "%s[%s]" x (printA a)
    | PlusExpr (a1, a2) -> sprintf "%s + %s" (printA a1) (printA a2)
    | MinusExpr (a1, a2) -> sprintf "%s - %s" (printA a1) (printA a2)
    | TimesExpr (a1, a2) -> sprintf "%s * %s" (printA a1) (printA a2)
    | ModExpr (a1, a2) -> sprintf "%s %% %s" (printA a1) (printA a2)
    | DivExpr (a1, a2) -> sprintf "%s / %s" (printA a1) (printA a2)
    | UPlusExpr (a) -> sprintf "+%s" (printA a)
    | UMinusExpr (a) -> sprintf "-%s" (printA a)
    | PowExpr (a1, a2) -> sprintf "%s ^ %s" (printA a1) (printA a2)

let rec printB b : string = 
    match b with
    | Bool(x) -> sprintf "%b" x
    | AndOp(b1, b2) -> sprintf "( %s ) & ( %s )" (printB b1) (printB b2)
    | OrOp(b1, b2) -> sprintf "( %s ) | ( %s )" (printB b1) (printB b2)
    | ForAllOp(b1, b2) -> sprintf "( %s ) && ( %s )" (printB b1) (printB b2)
    | ExistsOp (b1, b2) -> sprintf "( %s ) || ( %s )" (printB b1) (printB b2)
    | NotOp (b) -> sprintf "!( %s )" (printB b) 
    | EqualOp (a1, a2) -> sprintf "( %s ) = ( %s )" (printA a1) (printA a2)
    | NEqualOp (a1, a2) -> sprintf "( %s ) != ( %s )" (printA a1) (printA a2)
    | GreaterOp (a1, a2) -> sprintf "( %s ) > ( %s )" (printA a1) (printA a2)
    | GreaterEqualOp (a1, a2) -> sprintf "( %s ) >= ( %s )" (printA a1) (printA a2)
    | LessOp (a1, a2) -> sprintf "( %s ) < ( %s )" (printA a1) (printA a2)
    | LessEqualOp (a1, a2) -> sprintf "( %s ) <= ( %s )" (printA a1) (printA a2)

let printC c : string = 
    match c with
    | AssignStatement(x, a) -> sprintf "%s:=%s" x (printA a)
    | AssignArrayStatement(x, i, a) -> sprintf "%s[%s]:=%s" x (printA i) (printA a)
    | SkipStatement -> "skip"
    | _ -> sprintf "%A" c


let printPG (pg : ProgramGraph) : string = 
    let edgetostring = function
        | EdgeB(q0, b, q1) -> 
            sprintf "q%i -> q%i [label=\"%s\"]" q0 q1 ((printB b).Replace("\"", ""))
        | EdgeC(q0, c, q1) -> 
            sprintf "q%i -> q%i [label=\"%s\"]" q0 q1 ((printC c).Replace("\"", ""))
        

    let pgstring = List.foldBack (fun edge s -> sprintf "%s\n%s" (edgetostring edge) s) pg ""

    sprintf "digraph G {\n%s}\n" (pgstring)

let printMem mem : string =
    let varToString var : string =
        match var with 
        | Variable(f) -> sprintf "%g" f
        | ArrayVariable (array) -> 
            Seq.foldBack(fun k rest -> sprintf "%g, %s" (Map.find k array) rest) (Map.keys array) ""   
    
    let printField k s = 
        sprintf "%s: %s\n%s" k (varToString (Map.find k mem)) s

    Seq.foldBack printField (Map.keys mem) ""