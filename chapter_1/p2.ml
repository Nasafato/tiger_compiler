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


type assignment = id * int
type table = assignment list

(*// a := 5 + 3; b := ( print (a, a-1), 10 * a); print(b)*)
(*// prog = a:= 5 + 3;*)

type linkedList =
    | EndNode
    | Node of (id * int) * linkedList

let addToLinkedList ll id value =
    match ll with
    | EndNode ->
        Node( (id, value), EndNode )
    | Node _ ->
        Node( (id, value), ll )

let rec printLinkedList ll =
    match ll with
    | EndNode -> ()
    | Node ( (i, v), rest) ->
        let () = Printf.printf "%s = %i\n" i v in
        printLinkedList rest

let () =
    let l1 = addToLinkedList EndNode "a" 3 in
    let l2 = addToLinkedList l1 "b" 5 in
    let l3 = addToLinkedList l2 "a" 5 in
    let l4 = addToLinkedList l3 "c" 6 in
    printLinkedList l4

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

let update (table) (arg:assignment) =
    match table with
    | [] ->
        arg::table
    | x::xs ->
        arg::x::xs

let rec findInList boolFunc l =
    match l with
    | [] -> None
    | x::xs ->
        match (boolFunc x) with
        | true -> Some x
        | false -> findInList boolFunc xs

let lookup (table:table) (targetId) : int option =
    match List.find ~f:(fun (id, _) -> id = targetId) table with
    | None -> None
    | Some (_, resultValue) -> Some(resultValue)

let rec interpStmt stmt (table:table) =
    match stmt with
    | CompoundStmt (s1, s2) ->
        interpStmt s2 (interpStmt s1 table)
    | AssignStmt (id, exp) ->
        begin
        match interpExp exp table with
        | x, newTable ->
            update newTable (id, x)
        end
    | PrintStmt (expList) ->
        expList
        |> List.fold ~f:(fun table exp ->
            match exp with
            | IdExp id ->
                begin
                match lookup table id with
                | None -> Printf.printf "id '%s' has not been assigned yet\n" id; table
                | Some x -> Printf.printf "%d\n" x; table
                end
            | NumExp x -> Printf.printf "%d\n" x; table
            | _ ->
                begin
                match interpExp exp table with
                | _, newTable -> newTable
                end ) ~init:table
and interpExp (exp) (table):(int * table) =
    match exp with
    | IdExp _ -> (0, table)
    | NumExp x -> (x, table)
    | OpExp (e1, binOp, e2) ->
        let x, t1 = interpExp e1 table in
        let y, t2 = interpExp e2 t1 in
        begin
        match binOp with
        | Plus -> (x + y, t2)
        | Minus -> (x - y, t2)
        | Times -> (x * y, t2)
        | Div -> (x / y, t2)
        end
    | EseqExp (stmt, exp) ->
        let newTable = interpStmt stmt table in
        let x, newTable = interpExp exp newTable in
        (x, newTable)

let () =
    let result = maxArgs prog in
    Printf.printf "Max args is %i\n" result


let () =
    let outputTable = interpStmt prog [] in
    outputTable
    |> List.iter ~f:(fun (id, value) ->
        Printf.printf "%s = %d\n" id value
    )
