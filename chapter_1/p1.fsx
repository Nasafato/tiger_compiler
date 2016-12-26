type Id = string

type BinOp = Plus | Minus | Times | Div

type Stmt =
    | CompoundStmt of Stmt * Stmt
    | AssignStmt of Id * Exp
    | PrintStmt of Exp list
and Exp =
    | IdExp of Id
    | NumExp of int
    | OpExp of Exp * BinOp * Exp
    | EseqExp of Stmt * Exp

// a := 5 + 3; b := ( print (a, a-1), 10 * a); print(b)
// prog = a:= 5 + 3;

let prog =
    CompoundStmt(AssignStmt("a", OpExp(NumExp 5, Plus, NumExp 3)),
        CompoundStmt(AssignStmt("b", EseqExp(PrintStmt [IdExp "a"; OpExp(IdExp "a", Minus, NumExp 1)], OpExp(NumExp 10, Times, IdExp "a"))),
            PrintStmt [IdExp "b"]))


let rec maxArgs (statement:Stmt): int =
    match statement with
    | CompoundStmt(s1, s2) ->
        max (maxArgs s1) (maxArgs s2)
    | AssignStmt(id, exp) ->
        maxExpArgs exp
    | PrintStmt expList ->
        expList
        |> List.map maxExpArgs
        |> List.max
        |> max expList.Length
and maxExpArgs (exp:Exp): int =
    match exp with
    | IdExp id -> 0
    | NumExp num -> 0
    | OpExp(e1, _, e2) ->
        max (maxExpArgs e1) (maxExpArgs e2)
    | EseqExp(statement, exp) ->
        max (maxArgs statement) (maxExpArgs exp)

printf "Max args is %i\n" (maxArgs prog)
