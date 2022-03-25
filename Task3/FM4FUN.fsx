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

// Order is important i guess ... 

#load "FMEval.fs"
open FMEval

#load "FMTest.fs"
open FMTest

#load "FMGraph.fs"
open FMGraph

#load "FMPrint.fs"
open FMPrint

#load "FMStep.fs"
open FMStep


open System.Collections.Generic


let parse input =
    // translate string into a buffer of characters
    let lexbuf = LexBuffer<char>.FromString input
    // translate the buffer into a stream of tokens and parse them
    let res = FM4FUNParser.start FM4FUNLexer.tokenize lexbuf
    // return the result of parsing (i.e. value of type "expr")
    res


// Test a given testgcl 
let testGCL ( gcl : TestGCL ) =
    let (name, code, memin, memout) = gcl

    try
    // Print result
    let c = parse code
    

    // Run code and get memory
    let pg = graph c false
    let pm = getMap pg
    let (q, mem) = evalN 1000 0 pm memin


    printfn "Result of %s: %A" name mem
    
    printfn "Pretty print: "
//    printfn "%A" c
//    printfn "%s" (printCTree c)
//    printfn "%A" (graph c false)

    printfn "%s" (printPG pg)
//    printfn "%A" pm
    printfn "%i - %A" q mem

    // Test the memory
    let passed = testmem mem memout
    printf "Test passed: %b\n" passed 
    if not passed then printf "%A != %A" mem memout
    
    with err -> printf "Could not parse %s" name
 
    printf "\n\n"


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
