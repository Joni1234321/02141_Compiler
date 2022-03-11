// This script implements our interactive FM4FUN

// We need to import a couple of modules, including the generated lexer and parser
#r "FsLexYacc.Runtime.10.0.0/lib/net46/FsLexYacc.Runtime.dll"
open FSharp.Text.Lexing
open System
#load "FM4FUNTypesAST.fs"
open FM4FUNTypesAST
#load "FM4FUNParser.fs"
open FM4FUNParser
#load "FM4FUNLexer.fs"
open FM4FUNLexer

open System.Collections.Generic

// Vars are floats, and Arrays are stored as maps
type vtype = 
    | Variable of float
    | ArrayVariable of Map<int, float>
let defaultmem = Map.empty<string, vtype>

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

    | MultipleStatement (c1, c2) -> evalc c2 (evalc c1 mem)
    | IfStatement (gc) -> 
        let (executed, nextmem) = evalgc gc mem
        nextmem
    | DoStatement (gc) -> 
        let (executed, nextmem) = evalgc gc mem
        // if the previous iteration were executed then repeat
        if executed then evalc (DoStatement(gc)) nextmem
        else mem
    | SkipStatement -> mem

// gc -> mem -> (bool * mem)
and evalgc statement mem =
    match statement with
    | ThenStatement (b, c) -> 
        if (evalb b mem) then (true, evalc c mem) 
        else (false, mem)
    | ElseStatement (gc1, gc2) -> 
        let (executed, nextmem) = evalgc gc1 mem
        if (executed) then (true, nextmem)
        else evalgc gc2 mem

let parse input =
    // translate string into a buffer of characters
    let lexbuf = LexBuffer<char>.FromString input
    // translate the buffer into a stream of tokens and parse them
    let res = FM4FUNParser.start FM4FUNLexer.tokenize lexbuf
    // return the result of parsing (i.e. value of type "expr")
    res


// Only test if all defined variables in memtest exists in mem
let testmem (mem : Map<'a, 'b>) (memtest : Map<'a, 'b>) : bool = 
    Seq.forall2 (fun id value -> 
        match mem.TryFind(id) with 
        | Some(k) when k = value -> true
        | _ -> false) (Map.keys memtest) (Map.values memtest)

let testGCL gcl =
    let (name, code, memin, memout) = gcl

    // Print result
    try
    let c = parse code

    let mem = (evalc c memin)
    printfn "Result of %s: %A" name mem
    
    printfn "%A" c

    // Test the memory
    let passed = testmem mem memout
    printf "Test passed: %b\n" passed 
    if not passed then printf "%A != %A" mem memout
    
    with err -> printf "Could not parse %s" name
 
    printf "\n\n"


// name * code * memin * memexpectedout
type TestGCL = (string * string * Map<string, float> * Map<string, float>)
let buildArrayVariable (ls : float list) =
    let rec ba ls0 map i =
        match ls0 with
        | head :: tail -> 
            let nextmap = Map.add i head map 
            ba tail nextmap (i+1)
        | _ -> map
    ArrayVariable(ba ls Map.empty<int, float> 0)

let factorialGCL = (
    "Factorial ",
    "
    x:=4;
    y:=1;
    do x>0 -> y:=x*y;
            x:=x-1
    od
    ",
    defaultmem, 
    Map ["y",Variable(24.0)]
)

let maximalGCL = ( 
    "Maximal",
    "
    y:=11;
    x:=10;
    if x>=y -> z:=x
    [] y>x -> z:=y
    fi
    ",
    defaultmem,
    Map ["z",Variable(11.0)]
)

let arrayToSort = [10.;0.;12.;-2.;100.;-30.;3.]
let insertionsortGCL = (
    "Insertion Sort",
    "
    i:=1;
    do i<n -> j:=i;
            do (j>0)&&(A[j-1]>A[j]) -> t:=A[j];
                                        A[j]:=A[j-1];
                                        A[j-1]:=t;
                                        j:=j-1
            od;
            i:=i+1
    od
    ",
    Map ["A", buildArrayVariable arrayToSort; "n", Variable(float(arrayToSort.Length))],
    Map ["A", buildArrayVariable (List.sort arrayToSort)]
)

let arrayToAvg = [0.; 10. ; 20. ; 30.]
let getAvg (ls :float list) : float = 
    let sum = List.foldBack (fun e acc -> e + acc) ls 0.0
    sum / float(ls.Length) 

let averageGCL = (
    "Average of entries in array",
    "
    i:=0;
    x:=0;
    y:=0;
    do (n>i)&&(A[i]>=0) -> x:=x+A[i];
                        y:=y+1;
                        i:=i+1
    [] (n>i)&&(0>A[i]) -> i:=i+1
    od;
    x:=x/y
    ",
    Map ["A", buildArrayVariable arrayToAvg; "n", Variable(float(arrayToAvg.Length))],
    Map ["x", Variable(getAvg arrayToAvg)]
)



// We implement here the function that interacts with the user
let rec compute n =
    if n = 0 then
        printfn "Bye bye"
    else
        printf "Enter an arithmetic expression: "
        try
        // We parse the input string
        let c = parse (Console.ReadLine())
        
        printfn "Tree is %A" c

        // and print the result of evaluating it
        //printfn "Result: %f" (evalb (e) mem)
        printfn "Result: %A" (evalc (c) defaultmem)
        compute n
        with err -> compute (n-1)

testGCL factorialGCL 
testGCL maximalGCL
testGCL insertionsortGCL
testGCL averageGCL

// Start interacting with the user
compute 3
