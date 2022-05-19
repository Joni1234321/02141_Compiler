module FMSignAnalysis



type Sign = 
    | Positive
    | Negative
    | Zero

type PowerSet = (string * Sign) list
type AbstractMemory = Map<string, Set<Sign>> 


let signToIndex = function 
    | Negative -> 0
    | Zero     -> 1
    | Positive -> 2

type OperatorMatrix = Sign list list list
let plusMatrix : OperatorMatrix = [
    [[Negative]; [Negative]; [Negative; Zero; Positive]];
    [[Negative]; [Zero]; [Positive]];
    [[Negative; Zero; Positive]; [Positive] ; [Positive]]]

let timesMatrix : OperatorMatrix = [
    [[Positive]; [Zero]; [Negative]]; 
    [[Zero]; [Zero]; [Zero]]; 
    [[Negative]; [Zero]; [Positive]]]

let minusMatrix : OperatorMatrix = [
    [[Negative; Zero; Positive]; [Negative]; [Negative]];
    [[Positive]; [Zero]; [Negative]];
    [[Positive]; [Positive] ; [Negative; Zero; Positive]]]


let getOperatorSign signs1 signs2 (matrix : OperatorMatrix) : Set<Sign> = 
    Set.foldBack (fun s1 signSet -> 
        Set.union signSet (Set.foldBack (fun s2 signSet -> 
            Set.union signSet (Set.ofList (matrix[signToIndex s1][signToIndex s2]))) signs2 Set.empty)) signs1 Set.empty 
    

let rec applyAbstractA (a : a) (m1 : AbstractMemory, m2 : AbstractMemory) : Set<Sign> =
    let rec applyA a = applyAbstractA a (m1, m2)
    match a with 
    | Num(n) ->
        let sign =  match Math.Sign(n) with 
                    | -1 -> Negative
                    | 0 -> Zero
                    | 1 -> Positive
                    | _ -> failwith "not valid sign, BUT THIS SHOULD NOT HAPPEN"
        Set.empty.Add sign
    | Var(s) -> Map.find s m1
    | PlusExpr (a1, a2) -> getOperatorSign (applyA a1) (applyA a2) plusMatrix
    | TimesExpr (a1, a2) -> getOperatorSign (applyA a1) (applyA a2) timesMatrix
    | MinusExpr (a1, a2) -> getOperatorSign (applyA a1) (applyA a2) minusMatrix
    // TODO: add rest of operators
    |_ -> Set.ofList [Negative; Positive] 
    
// TODO: Implement this shit
// Skal man apply x -> s før eller efter man applyer abstract a
let semanticsApply (alpha : Alpha) (powerSet : Set<PowerSet>) : Set<PowerSet> = 
    match alpha with 
    | AlphaC(AssignStatement(s, a)) -> 
        let finalSet = applyAbstractA a (powerSet, powerSet)
        Set.foldBack () finalSet
    | _ ->  Set.empty

let signAnalysis (pg : ProgramGraph) (signs : PowerSet) : Map<int, Set<PowerSet>> =
    // Populate the output (remember to start and stop nodes)
    let initialA = Map.empty.Add(0, Set.empty.Add signs).Add(-1, Set.empty)
    let mutable A = List.foldBack(fun (q , _, _) A -> if q = 0 then A else Map.add q (Set.empty) A) pg initialA
    
    let mutable W = Set.empty.Add 0

    while not (Set.isEmpty W) do
        let q = List.head (Set.toList W)
        W <- Set.remove q W
        List.foldBack(
            fun (qstart, alpha, qend) A ->
                if qstart = q then
                    let (nextPowerSet, powerSetsQend) = (semanticsApply alpha (Map.find qstart A), Map.find qend A)
                    if not (Set.isSubset nextPowerSet powerSetsQend) then 
                        W <- Set.union W (Set.empty.Add qend)
                        Map.add qend (Set.union powerSetsQend nextPowerSet) A
                    else A
                else A) pg A
    // 
    printfn "W = %A" W

    A
// The algorithm
// FOR ALL nodes that are not the stat node, make an empty array A[q] = []
// A(q0) = signs
// W = [q0]
// while W is not empty
//      q = w.pop() // Remember to remove q from W
//      for all edges in the program graph where startnode is q
//          let (qstart, alpha, qend)
//          let nextSign = CalculateNewSigns(alpha A(qstart))
//          if nextSign is not a subset of A(qend)
//              A(qend) = A(qend).union (nextSign)
//              w = W.union(qend)
