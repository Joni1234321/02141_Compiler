module FMPredicate

type e =
    | LogicalVar    of string
    | ProgramVar    of string
    | PlusExpr      of (e * e)
    | MinusExpr     of (e * e)
    | TimesExpr     of (e * e)
    | ModExpr       of (e * e)
    | DivExpr       of (e * e)
    | UPlusExpr     of (e)
    | UMinusExpr    of (e)
    | PowExpr       of (e * e)
    | MathFunction  of (string * e list)

type Predicate =
    | BoolPred          of bool
    | AndPred           of (Predicate * Predicate)
    | OrPred            of (Predicate * Predicate)
    | NotPred           of Predicate
    | IfPred            of (Predicate * Predicate)
    | ExistsPred        of (string * Predicate)
    | ForAllPred        of (string * Predicate)
    | EqualPred         of (e * e)
    | NEqualPred        of (e * e)
    | GreaterPred       of (e * e)
    | GreaterEqualPred  of (e * e)
    | LessPred          of (e * e)
    | LessEqualPred     of (e * e)
    | CustomPred        of (string * e list)