open Syntax;;

let rec position (x : string) (venv : string list) : int =
match venv with
    | [] -> failwith (x^":no matching variable in environment")
    | y::venv2 -> if x=y then 0 else (position x venv2) + 1

let rec compile e venv = 
    let cmp2 e1 e2 = 
        (compile e2 venv) 
        @ (compile e1 venv)
    in
    match e with
    | Var(x) -> [CAM_Access(position x venv)]
    | IntLit(n) -> [CAM_Ldi(n)]
    | BoolLit(b) -> [CAM_Ldb(b)]
    | If(e1, e2, e3) ->
        (compile e1 venv)
        @ [CAM_Test(
            compile e2 venv,
            compile e3 venv
        )]
    | Let(x,e1,e2) -> 
        (compile e1 venv)
        @ [CAM_Let]
        @ (compile e2 (x::venv))
        @ [CAM_EndLet]
    | LetRec(f, x, e1, e2) -> 
        [CAM_Closure(
            (compile e1 (x::f::venv))
            @ [CAM_Return]
        ); CAM_Let]
        @ (compile e2 (f::venv))
        @ [CAM_EndLet]
    | Fun(x,e1) -> 
        [CAM_Closure(
            (compile e1 (x::"_"::venv))
            @ [CAM_Return]
        )]
    | App(e1, e2) ->
        (cmp2 e1 e2)
        @ [CAM_Apply]
    | Eq(e1, e2) ->
        (cmp2 e1 e2)
        @ [CAM_Eq]
    | NotEq(e1, e2) ->
        (cmp2 e1 e2)
        @ [CAM_NotEq]
    | Greater(e1, e2) ->
        (cmp2 e1 e2)
        @ [CAM_Greater]
    | Less(e1, e2) ->
        (cmp2 e1 e2)
        @ [CAM_Less]
    | Plus(e1, e2) ->
        (cmp2 e1 e2)
        @ [CAM_Add]
    | Minus(e1, e2) ->
        (cmp2 e1 e2)
        @ [CAM_Minus]
    | Times(e1, e2) ->
        (cmp2 e1 e2)
        @ [CAM_Times]
    | Div(e1, e2) ->
        (cmp2 e1 e2)
        @ [CAM_Div]
    | Reminder(e1, e2) ->
        (cmp2 e1 e2)
        @ [CAM_Reminder]
    | _ -> failwith "compile error"