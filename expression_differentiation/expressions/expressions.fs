module expressions

type Expr =
    | Var
    | Num of int
    | Sum of Expr * Expr
    | Prod of Expr * Expr

let rec deriv expr =
    match expr with
    | Var -> Num 1
    | Num _ -> Num 0
    | Sum (e1, e2) -> Sum (deriv e1, deriv e2)
    | Prod (e1, e2) -> Sum (Prod (e1, deriv e2), Prod (e2, deriv e1))

let precSum = 10
let precProd = 20
let rec stringOfExpr prec expr =
    match expr with
    | Var -> "x"
    | Num i -> i.ToString()
    | Sum (e1, e2) ->
        let sum = stringOfExpr precSum e1 + "+" + stringOfExpr precSum e2
        if prec > precSum then
            "(" + sum + ")"
        else
            sum
    | Prod (e1, e2) ->
        stringOfExpr precProd e1 + "*" + stringOfExpr precProd e2

let printExpr = stringOfExpr precSum

let simpSum (a, b) =
    match a, b with
    | Num n, Num m -> Num (n + m)
    | Num 0, e | e, Num 0 -> e
    | e1, e2 -> Sum (e1, e2)

let simpProd (a, b) =
    match a, b with
    | Num n, Num m -> Num (n * m)
    | Num 0, e | e, Num 0 -> Num 0
    | Num 1, e | e, Num 1 -> e
    | e1, e2 -> Prod(e1, e2)

let rec simpDeriv e =
    match e with
    | Var -> Num 1
    | Num _ -> Num 0
    | Sum (e1, e2) -> simpSum (simpDeriv e1, simpDeriv e2)
    | Prod (e1, e2) -> simpSum (simpProd (e1, simpDeriv e2),
                                simpProd (e2, simpDeriv e1))

[<EntryPoint>]
let main argv =
    let e1 = Sum (Num 1, Prod (Num 2, Var))
    let e3 = Prod (Var, Prod (Var, Num 2))
    printfn "%s" (stringOfExpr precSum e1)
    printfn "%s" (stringOfExpr precSum (deriv e1))
    printfn "%s" (printExpr e3)
    printfn "%s" (printExpr (deriv e3))
    printfn "%A" (simpDeriv e3)

    0 // return an integer exit code



