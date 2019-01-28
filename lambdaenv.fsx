#!/usr/bin/env fsharpi
// Learn more about F# at http://fsharp.org

type Value =
    | Int of int
    | Closure of (Value -> Value)
    | Product of Value list

type Var = string

type Op = string

type Lit =
    | Int of int

type Expr =
    | Lit of Lit
    | Let of Var * Expr * Expr
    | If of Expr * Expr * Expr
    | LetRec of Var * Var * Expr * Expr
    | Apply of Var * Expr
    | Var of Var
    | Primop of Op * Expr list
    | Product of Expr list
    | Select of Var * int

module Env =

    let empty = List.empty

    let find k env = List.find (fun (k', _) -> k = k') env |> snd

    let add k v env = (k, v) :: env

    let rec show env =
        match env with
        | (k, v) :: env -> (sprintf "%O => %O; " k v) + show env
        | [] -> "__"

let eval expr =
    let rec eval expr env =
        match expr with
        | Lit (Int i) -> Value.Int i
        | Product p ->
            p
            |> List.map (fun expr -> eval expr env)
            |> Value.Product
        | Var v -> Env.find v env
        | Let (v, e1, e2) ->
            let e1 = eval e1 env
            let env = Env.add v e1 env
            eval e2 env
        | If (e1, e2, e3) ->
            match eval e1 env with
            | Value.Int 1 -> eval e2 env
            |  Value.Int 0 -> eval e3 env
            | e -> failwithf "Not a bool: %O" e
        | Apply (fn, arg) ->
            match Env.find fn env with
            | Closure fn ->
                let arg = eval arg env
                fn arg
            | e -> failwithf "Not a closure: %O" e
        | LetRec (fn, arg, e1, e2) ->
            let rec f env arg =
                let rec closure v =
                    let env = Env.add arg v env
                    eval e1 (g env)
                Closure (closure)
            and g env = Env.add fn (f env arg) env
            eval e2 (g env)
        | Primop (("+"|"-"| "="|"<"|"*") as op, [e1; e2]) ->
            match eval e1 env with
            | Value.Int i ->
                match eval e2 env with
                | Value.Int j ->
                    match op with
                    | "+" -> Value.Int (i + j)
                    | "-" -> Value.Int (i - j)
                    | "*" -> Value.Int (i * j)
                    | "<" ->
                        let b = if (i < j) then 1 else 0
                        Value.Int b
                    | "=" ->
                        let b = if (i = j) then 1 else 0
                        Value.Int b
                | e -> failwithf "Not an int: %O" e
            | e -> failwithf "Not an int: %O" e
        | Select (p, i) ->
            match Env.find p env with
            | Value.Product p -> p.[i]
            | e -> failwithf "Not an int: %O" e
        | e -> failwithf "Not a valid expr: %O" e
    eval expr Env.empty

<@ (+) 21 21 @> |> ignore
do printfn "%O" (eval (Primop("+", [Lit (Int 21); Lit (Int 21)])))

<@ let n = 21 + 21 in n @> |> ignore
do
    printfn "%O" (
        eval (
            Let ("n", Primop("+", [Lit (Int 21); Lit (Int 21)]), Var "n")
    ))

<@ if true then 1 else 0 @>
do
    printfn "%O" (
        eval (
            If (
                (Lit (Int 1)),
                (Lit (Int 1)),
                (Lit (Int 0))
    )))

<@
    let rec fact n =
        if n < 1 then 1
        else n * fact (n - 1)
    in fact 5
@> |> ignore
do

    let fn = "fact"
    let arg = "n"
    let one =  Lit (Int 1)
    let e1 =
        let e1 =
            Primop(
                "<",
                [
                    Var arg
                    one
                ]
            )
        let e2 = one
        let e3 =
            Primop(
                    "*",
                    [
                        Var "n"
                        Apply (
                            fn,
                            Primop(
                                "-",
                                [
                                    Var arg
                                    one
                                ]
                            )
                        )
                    ]
                )
        If (e1, e2, e3)
    let e2 = Apply (fn, Lit (Int 5))
    let letrec = LetRec (fn, arg, e1, e2)
    let e = eval letrec
    printfn "%O" e

<@
    let x = 1
    let y = 1
    let p = (x, y) in
    fst p + snd p
@> |> ignore
do
    let e =
        Let (
            "x",
            Lit (Int 1),
            Let (
                "y",
                Lit (Int 1),
                Let (
                    "p",
                    Product
                        [
                            Var "x"
                            Var "y"
                        ],
                    Primop(
                        "+",
                        [
                            Select (
                                "p",
                                0
                            )
                            Select (
                                "p",
                                1
                            )
                        ]
                    )
                )
            )
        )
    printfn "%O" (eval e)
