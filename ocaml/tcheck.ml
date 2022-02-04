open Syntax;;

let ext env x v = (x,v) :: env

let rec lookup x env =
  match env with
  | [] -> failwith ("unbound variable: " ^ x)
  | (y,v)::tl -> if x=y then v 
      else lookup x tl 

(* tcheck : tyenv -> exp -> ty *)
let rec tcheck te e =
    let bop_i te e1 e2 = 
        begin
            match (tcheck te e1, tcheck te e2) with
            | (TInt, TInt) -> TInt
            | _ -> failwith "Integer value expected"
        end
    in
    let bop_b te e1 e2 = 
        let t1 = tcheck te e1 in
        let t2 = tcheck te e2 in
        begin
            match t1=t2 with
            | true -> TBool
            | _ -> failwith "Comparing different types"
        end 
    in
    match e with
    | Var(s)       ->  lookup s te   (* 変数の型は、型環境 te から取ってくる *)
    | IntLit(_)    -> TInt
    | BoolLit(_)   -> TBool
    | If(e1,e2,e3) -> 
        let t1 = tcheck te e1 in
        let t2 = tcheck te e2 in
        let t3 = tcheck te e3 in
        begin
        match (t1, t2=t3) with
            (TBool, true) -> t2
        | _ -> failwith "type error in IF" 
        end
    | Fun(x, e1) ->
        let t1 = lookup x te in
        let t2 = tcheck te e1 in 
        TArrow(t1,t2)
    | App(e1,e2) ->
        let t1 = tcheck te e1 in
        let t2 = tcheck te e2 in 
        begin
            match t1 with
            | TArrow(t10,t11) -> if t2=t10 then t11
            else failwith "type error in App"
            | _ -> failwith "type error in App"
        end
    | Eq(e1, e2) ->
        (bop_b te e1 e2)
    | NotEq(e1, e2) ->
        (bop_b te e1 e2)
    | Greater(e1, e2) ->
        (bop_b te e1 e2)
    | Less(e1, e2) ->
        (bop_b te e1 e2)
    | Plus(e1,e2)  -> 
        (bop_i te e1 e2)
    | Minus(e1,e2) ->
        (bop_i te e1 e2)
    | Times(e1,e2)  -> 
        (bop_i te e1 e2)
    | Div(e1, e2) -> 
        (bop_i te e1 e2)
    | Reminder(e1, e2) ->
        (bop_i te e1 e2)
    | Empty -> Empty
    | Cons(e1, e2) ->
        begin
            match e2 with
            | Empty -> TList(tcheck te e1)
            | Cons(e3, _) ->
                let t1 = tcheck te e1
                in
                let t3 = tcheck te e3
                in
                if t1=t3 then TList(t1) 
                else failwith "type error in Cons"
            | _ -> failwith "type error in Cons"
        end
    | Head(e1) -> 
        begin
            match tcheck te e1 with
            | TList(t1) -> t1
            | _ -> failwith "type error in Head"
        end
    | Tail(e1) -> 
        begin
            match tcheck te e1 with
            | TList(t1) ->
                begin
                    match e1 with
                    | Cons(_, e3) -> 
                        if e3=Empty then Empty
                        else t1
                    | _ -> failwith "type error in Tail"
                end
            | _ -> failwith "type error in Tail"
        end
    | _ -> failwith "unknown expression"