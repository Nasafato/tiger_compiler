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

type Assignment =  Id * int

type Table = Assignment seq

let prog =
    CompoundStmt(AssignStmt("a", OpExp(NumExp 5, Plus, NumExp 3)),
        CompoundStmt(AssignStmt("b", EseqExp(PrintStmt [IdExp "a"; OpExp(IdExp "a", Minus, NumExp 1)], OpExp(NumExp 10, Times, IdExp "a"))),
            PrintStmt [IdExp "b"]))

let update (table:Table) (assignment:Assignment):(Table) =
    seq {yield assignment; yield! table}

let lookup (table:Table) (targetId: Id):(Option<int>) =
    let result =
        table
        |> Seq.tryFind (fun (id, value) ->
            id = targetId )

    match result with
    | None -> None
    | Some (_, resultValue) -> Some(resultValue)
let rec interpStmt (statement:Stmt) (table:Table):(Table) =
    match statement with
    | CompoundStmt(s1, s2) ->
        interpStmt s2 (interpStmt s1 table)
    | AssignStmt(id, exp) ->
        match interpExp exp table with
        | x, newTable ->
            update newTable (id, x)
    | PrintStmt(expList) ->
        expList
        |> Seq.fold (fun table exp ->
            match exp with
            | IdExp id ->
                match lookup table id with
                | None -> printf "id '%s' has not been assigned yet\n" id; table
                | Some value -> printf "%i\n" value; table
            | NumExp x -> printf "%i" x; table
            | _ ->
                match interpExp exp table with
                | _, newTable -> newTable
        ) table
and interpExp (expression:Exp) (table:Table):(int * Table) =
    match expression with
    | IdExp id -> (0, table)
    | NumExp x -> (x, table)
    | OpExp(e1, binOp, e2) ->
        let x, t1 = interpExp e1 table
        let y, t2 = interpExp e2 t1
        match binOp with
        | Plus -> (x + y, t2)
        | Minus -> (x - y, t2)
        | Times -> (x * y, t2)
        | Div -> (x / y, t2)
    | EseqExp(stmt, exp) ->
        let newTable = interpStmt stmt table
        let x, newTable = interpExp exp newTable
        (x, newTable)

let outputTable = interpStmt prog (Seq.empty)

outputTable
|> Seq.iter( fun (id, value) ->
    printf "%s = %i\n" id value
)