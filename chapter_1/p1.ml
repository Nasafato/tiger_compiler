open Core.Std


type id = string
type binOp = Plus | Minus | Times | Div

type stmt =
    | CompoundStmt of stmt * stmt
    | AssignStmt of id * exp
    | PrintStmt of exp list
and exp =
    | IdExp of id
    | NumExp of int
    | OpExp of exp * binOp * exp
    | EseqExp of stmt * exp

(*// a := 5 + 3; b := ( print (a, a-1), 10 * a); print(b)*)
(*// prog = a:= 5 + 3;*)

let prog =
    CompoundStmt(AssignStmt("a", OpExp(NumExp 5, Plus, NumExp 3)),
        CompoundStmt(AssignStmt("b", EseqExp(PrintStmt [IdExp "a"; OpExp(IdExp "a", Minus, NumExp 1)], OpExp(NumExp 10, Times, IdExp "a"))),
            PrintStmt [IdExp "b"]))

let my_max l =
    List.fold l ~init:0 ~f:(fun acc x -> max acc x)

let rec maxArgs (statement:stmt): int =
    match statement with
    | CompoundStmt(s1, s2) ->
        max (maxArgs s1) (maxArgs s2)
    | AssignStmt(_, exp) ->
        maxExpArgs exp
    | PrintStmt expList ->
        expList
        |> List.map ~f:maxExpArgs
        |> my_max
        |> max (List.length expList)
and maxExpArgs (exp:exp): int =
    match exp with
    | IdExp _ -> 0
    | NumExp _ -> 0
    | OpExp(e1, _, e2) ->
        max (maxExpArgs e1) (maxExpArgs e2)
    | EseqExp(s, ex) ->
        max (maxArgs s) (maxExpArgs ex)

let () =
    let result = maxArgs prog in
    Printf.printf "Max args is %i\n" result