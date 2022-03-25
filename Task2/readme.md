# How to build
write "build" in cmd (windows)

Remember this requires that you have the right libraries, check the root readme.md on how to download them
# How to run 
write "run" in cmd (windows)

# How to build Graph
paste the graph code into pg.dot 
then write "pg" in cmd 
then open the pg.svg file
# What files 
.fsy is the parser  
.fsl is the lexer  
AST.fs is the Abstract Syntax Tree  
.fsx is the program you run ( the one that evalueates the syntax tree)


# The modules
**FMEval.fs**: Contains the evaluate functions  
**FMMemory.fs**: Contains the memory type and subtype  
**FMTest.fs**: Contains all helper functions used to make tests, as well as the tests themselves  
**FMGraph.fs**: Contains the code that converts it into a program graph
**FM4FUN.fsx**: The main file, contains the parser and all the main functions