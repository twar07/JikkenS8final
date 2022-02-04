open Syntax;;
open Parser;;

let ext env x v = (x,v) :: env
let rec lookup x env =
  match env with
  | [] -> failwith ("unbound variable: " ^ x)
  | (y,v)::tl -> if x=y then v 
      else lookup x tl 

let rec evaluation e env =
  let binop f e1 e2 env =
    match (evaluation e1 env, evaluation e2 env) with
    | (IntVal(n1),IntVal(n2)) -> IntVal(f n1 n2)
    | _ -> failwith "integer value expected"
  in 
  let binop_b f g e1 e2 env =
    match (evaluation e1 env, evaluation e2 env) with
    | (IntVal(n1), IntVal(n2)) -> BoolVal(f n1 n2)
    | (BoolVal(b1), BoolVal(b2)) -> BoolVal(g b1 b2)
    | _ -> failwith "wrong value"
  in 
  match e with
  | Var(x)       -> lookup x env 
  | IntLit(n)    -> IntVal(n)
  | BoolLit(b)  -> BoolVal(b)
  | Plus(e1,e2)  -> binop (+) e1 e2 env
  | Minus(e1,e2)  -> binop (-) e1 e2 env
  | Times(e1,e2) -> binop ( * ) e1 e2 env
  | Div(e1,e2)  -> binop (/) e1 e2 env
  | Reminder(e1,e2)  -> binop (mod) e1 e2 env
  | Eq(e1,e2)    -> binop_b (=) (=) e1 e2 env
  | NotEq(e1,e2)    -> binop_b (!=) (!=) e1 e2 env
  | Greater(e1,e2)    -> binop_b (>) (>) e1 e2 env
  | Less(e1,e2)    -> binop_b (<) (<) e1 e2 env
  | If(e1,e2,e3) ->
      begin
        match (evaluation e1 env) with
        | BoolVal(true)  -> evaluation e2 env
        | BoolVal(false) -> evaluation e3 env
        | _ -> failwith "wrong value"
      end
  | Let(x,e1,e2) ->
      let env1 = ext env x (evaluation e1 env) 
      in evaluation e2 env1
  | LetRec(f,x,e1,e2) ->
      let env1 = ext env f (RecFunVal (f, x, e1, env))
      in evaluation e2 env1
  | Fun(x,e1) -> FunVal(x, e1, env)
  | App(e1,e2) ->
      let funpart = (evaluation e1 env) in
      let arg = (evaluation e2 env) in
        begin
         match funpart with
         | FunVal(x,body,env1) ->
            let env2 = (ext env1 x arg) in
            evaluation body env2
         | RecFunVal(f,x,body,env1) ->
            let env2 = (ext (ext env1 x arg) f funpart) in
            evaluation body env2
         | _ -> failwith "wrong value in App"
        end
  | Empty -> ListVal([])
  | Cons(e1,e2) ->
    let rec sametype v1 v2 = 
      begin
        match (v1, v2) with
        | (IntVal(_), IntVal(_)) -> true
        | (BoolVal(_), BoolVal(_)) -> true
        | _ -> false
      end
    in
    let rec valcheck v1 v2 =
      begin
        match (v1, v2) with
        | (ListVal(l1), ListVal(l2)) -> valcheck (List.hd l1) (List.hd l2)
        | _ -> sametype v1 v2
      end
    in 
    begin
      match (evaluation e1 env, evaluation e2 env) with
      | (v1,ListVal(v2)) -> 
        begin
          match v2 with
            | [] -> ListVal(v1 :: v2)
            | _  ->
              if valcheck v1 (List.hd v2) then ListVal(v1 :: v2)
              else failwith "wrong value in Cons"
        end
      | _ -> failwith "wrong value in Cons"
    end
  | Head(e1)  ->
    begin 
      match (evaluation e1 env) with
      | ListVal(v1) ->
        begin
          match v1 with
          | hd::tl -> hd
          | _ -> failwith "empty list" 
        end
      | _ -> failwith "list expected."
    end
  | Tail(e1)  ->
    begin 
      match (evaluation e1 env) with
      | ListVal(v1) ->
        begin
          match v1 with
          | hd::tl -> ListVal(tl)
          | _ -> failwith "empty list" 
        end
      | _ -> failwith "list expected."
    end
  | _ -> failwith "unknown expression"