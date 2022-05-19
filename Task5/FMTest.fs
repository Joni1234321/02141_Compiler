module FMTest

// Test type
// name * code * memin * expectedmemory
type TestGCL = (string * string * Map<string, VType> * Map<string, VType>)

// Only test if all defined variables in memtest exists in mem
let testmem (mem : Map<'a, 'b>) (memtest : Map<'a, 'b>) : bool = 
    Seq.forall2 (fun id value -> 
        match mem.TryFind(id) with 
        | Some(k) when k = value -> true
        | _ -> false) (Map.keys memtest) (Map.values memtest)



// Helper function that generates an Array from a list
let buildArrayVariable (ls : float list) =
    let rec ba ls0 map i =
        match ls0 with
        | head :: tail -> 
            let nextmap = Map.add i head map 
            ba tail nextmap (i+1)
        | _ -> map
    ArrayVariable(ba ls Map.empty<int, float> 0)

// Tests
let factorialGCL = (
    "Factorial",
    "
    y:=1;
    do x>0 -> y:=x*y;
            x:=x-1
    od
    ",
    Map ["x", Variable(4.0)], 
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
    x:=x/y",
    Map ["A", buildArrayVariable arrayToAvg; "n", Variable(float(arrayToAvg.Length))],
    Map ["x", Variable(getAvg arrayToAvg)]
)
