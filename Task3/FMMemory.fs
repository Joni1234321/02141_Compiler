module FMMemory

// Vars are floats, and Arrays are stored as maps
type VType = 
    | Variable of float
    | ArrayVariable of Map<int, float>

// Default memory is empty, which requires all variable declaration to be made in the code
let defaultmem = Map.empty<string, VType>