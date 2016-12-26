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

let update (table:Table) (assignment:Assignment):(Table) =
    seq {yield assignment; yield! table}
let rec interpStmt (statement:Stmt) (table:Table):(Table) =
    match statement with
    | CompoundStmt(s1, s2) ->
        interpStmt s2 (interpStmt s1 table)
    | AssignStmt(id, exp) ->
        let x, newTable = interpExp exp table
        update newTable (id, x)
    | PrintStmt(expList) ->
        expList
        |> Seq.fold (fun table exp ->
            let _, newTable = interpExp exp table
            newTable
        ) table
and interpExp (expression:Exp) (table:Table):(int * Table) =
    3, seq {yield ("a", 3); yield ("b", 4)}