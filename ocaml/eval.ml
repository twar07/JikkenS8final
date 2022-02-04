open Syntax;;
open Parser;;

let emptyenv () = [] 
let ext env x v = (x,v) :: env
let rec lookup x env =
  match env with
  | [] -> failwith ("unbound variable: " ^ x)
  | (y,v)::tl -> if x=y then v 
      else lookup x tl 

let rec eval5 e env =
  let binop f e1 e2 env =
    match (eval5 e1 env, eval5 e2 env) with
    | (IntVal(n1),IntVal(n2)) -> IntVal(f n1 n2)
    | _ -> failwith "integer value expected"
  in 
  match e with
  | Var(x)       -> lookup x env 
  | IntLit(n)    -> IntVal(n)
  | BoolLit(b)  -> BoolVal(b)
  | Plus(e1,e2)  -> binop (+) e1 e2 env
  | Minus(e1,e2)  -> binop (-) e1 e2 env
  | Times(e1,e2) -> binop ( * ) e1 e2 env
  | Eq(e1,e2)    -> BoolVal((eval5 e1 env)=(eval5 e2 env))
  | If(e1,e2,e3) ->
      begin
        match (eval5 e1 env) with
        | BoolVal(true)  -> eval5 e2 env
        | BoolVal(false) -> eval5 e3 env
        | _ -> failwith "wrong value"
      end
  | Let(x,e1,e2) ->
      let env1 = ext env x (eval5 e1 env) 
      in eval5 e2 env1
  | LetRec(f,x,e1,e2) ->
      let env1 = ext env f (RecFunVal (f, x, e1, env))
      in eval5 e2 env1
  | Fun(x,e1) -> FunVal(x, e1, env)
  | App(e1,e2) ->
      let funpart = (eval5 e1 env) in
      let arg = (eval5 e2 env) in
        begin
         match funpart with
         | FunVal(x,body,env1) ->
            let env2 = (ext env1 x arg) in
            eval5 body env2
         | RecFunVal(f,x,body,env1) ->
            let env2 = (ext (ext env1 x arg) f funpart) in
            eval5 body env2
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
      match (eval5 e1 env, eval5 e2 env) with
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
      match (eval5 e1 env) with
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
      match (eval5 e1 env) with
      | ListVal(v1) ->
        begin
          match v1 with
          | hd::tl -> ListVal(tl)
          | _ -> failwith "empty list" 
        end
      | _ -> failwith "list expected."
    end
  | _ -> failwith "unknown expression"


(* open Syntax;;
open Parser;;


let rec position (x : string) (venv : string list) : int =
match venv with
    | [] -> failwith "no matching variable in environment"
    | y::venv2 -> if x=y then 0 else (position x venv2) + 1

let rec compile e venv = 
    match e with
    | Var(x) -> [CAM_Access(position x venv)]
    | Fun(x,e1) -> 
        [CAM_Closure(
            (compile e1 (x::"_"::venv))
            @ [CAM_Return]
        )]
    | App(e1, e2) ->
        (compile e2 venv)
        @ (compile e1 venv)
        @ [CAM_Apply]
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
    | IntLit(n) -> [CAM_Ldi(n)]
    | BoolLit(b) -> [CAM_Ldb(b)]
    | Plus(e1, e2) ->
        (compile e2 venv)
        @ (compile e1 venv)
        @ [CAM_Add]
    | Eq(e1, e2) ->
        (compile e2 venv)
        @ (compile e1 venv)
        @ [CAM_Eq]
    | If(e1, e2, e3) ->
        (compile e1 venv)
        @ [CAM_Test(
            compile e2 venv,
            compile e3 venv
        )]
    | _ -> failwith "compile error"


let rec interpret c e s =
    match c with
    | hd::c -> 
        begin
            match hd with
            | CAM_Ldi(n)    -> interpret c e (CAM_IntVal(n)::s)
            | CAM_Ldb(b)    -> interpret c e (CAM_BoolVal(b)::s)
            | CAM_Access(i) -> interpret c e ((List.nth e i)::s)
            | CAM_Closure(cc)    -> interpret c e (CAM_ClosVal(cc, e)::s)
            | CAM_Apply     -> 
                begin
                    match s with
                    | CAM_ClosVal(cc, ee)::v::s ->
                        interpret cc (v::CAM_ClosVal(cc,ee)::ee) (CAM_ClosVal(c,e)::s)
                    | _ -> failwith "error in Apply"
                end
            | CAM_Return    -> 
                begin
                    match s with
                    | v::CAM_ClosVal(cc,ee)::s ->
                        interpret cc ee (v::s)
                    | _ -> failwith "error in Return"
                end
            | CAM_Let       -> 
                begin
                    match s with
                    | v::s -> interpret c (v::e) s
                    | _ -> failwith "error in Let"
                end
            | CAM_EndLet    -> 
                begin
                    match e with
                    | v::env -> interpret c env s
                    | _ -> failwith "error in EndLet"
                end
            | CAM_Test (c1, c2) -> 
                begin
                    match s with
                    | CAM_BoolVal(true)::s -> interpret (c1@c) e s
                    | CAM_BoolVal(false)::s -> interpret (c2@c) e s
                    | _ -> failwith "error in Test"
                end
            | CAM_Add   -> 
                begin
                    match s with
                    | CAM_IntVal(n1)::CAM_IntVal(n2)::s -> interpret c e (CAM_IntVal(n1+n2)::s)
                    | _ -> failwith "error in Add"
                end
            | CAM_Eq    -> 
                begin
                    match s with
                    | n1::n2::s -> interpret c e (CAM_BoolVal(n1=n2)::s)
                    | _ -> failwith "error in Eq"
                end
            | CAM_Minus   -> 
                begin
                    match s with
                    | CAM_IntVal(n1)::CAM_IntVal(n2)::s -> interpret c e (CAM_IntVal(n1-n2)::s)
                    | _ -> failwith "error in Add"
                end
            | CAM_Times     ->
                begin
                    match s with
                    | CAM_IntVal(n1)::CAM_IntVal(n2)::s -> interpret c e (CAM_IntVal(n1*n2)::s)
                    | _ -> failwith "error in Add"
                end
            | CAM_Greater   ->
                begin
                    match s with
                    | CAM_IntVal(n1)::CAM_IntVal(n2)::s -> interpret c e (CAM_BoolVal(n1>n2)::s)
                    | _ -> failwith "error in Add"
                end
        end
    | []    -> 
        match s with
        | hd::tl -> hd
        | [] -> failwith "code expected"














let emptyenv () = [] 
let ext env x v = (x,v) :: env
let rec lookup x env =
  match env with
  | [] -> failwith ("unbound variable: " ^ x)
  | (y,v)::tl -> if x=y then v 
      else lookup x tl 

(* 「型」をあらわす型の定義 *)
type ty = TInt | TBool | TArrow of ty * ty

(* tcheck3 : tyenv -> exp -> ty *)
let rec tcheck3 te e =
  match e with
  | Var(s)       ->  lookup s te   (* 変数の型は、型環境 te から取ってくる *)
  | IntLit(_)    -> TInt
  | BoolLit(_)   -> TBool
  | Plus(e1,e2)  -> 
    begin
      match (tcheck3 te e1, tcheck3 te e2) with
        (TInt, TInt)  -> TInt
      | _ -> failwith "type error in Plus"
    end
  | Times(e1,e2)  -> 
    begin
      match (tcheck3 te e1, tcheck3 te e2) with
        (TInt, TInt)  -> TInt
      | _ -> failwith "type error in Plus"
    end
  | Eq(e1, e2) ->
		begin
			match (tcheck3 te e1, tcheck3 te e2) with
				(TInt,TInt) -> TBool
			| (TBool,TBool) -> TBool
			| _ -> failwith "type error in Eq"
		end
  | If(e1,e2,e3) -> 
    let t1 = tcheck3 te e1 in
    let t2 = tcheck3 te e2 in
    let t3 = tcheck3 te e3 in
    begin
      match (t1, t2=t3) with
        (TBool, true) -> t2
      | _ -> failwith "type error in IF" 
    end
  | Fun(x, e1) ->
    let t1 = lookup x te in
    let t2 = tcheck3 te e1 in 
      TArrow(t1,t2)
  | App(e1,e2) ->
    let t1 = tcheck3 te e1 in
    let t2 = tcheck3 te e2 in 
      begin
        match t1 with
        | TArrow(t10,t11) -> if t2=t10 then t11
          else failwith "type error in App"
        | _ -> failwith "type error in App"
      end
  | _ -> failwith "unknown expression"

(* tcheck2 : tyenv -> exp -> ty *)
let rec tcheck2 te e =
  match e with
  | Var(s)       ->  lookup s te   (* 変数の型は、型環境 te から取ってくる *)
  | IntLit(_)    -> TInt
  | BoolLit(_)   -> TBool
  | Plus(e1,e2)  -> 
    begin
      match (tcheck2 te e1, tcheck2 te e2) with
        (TInt, TInt)  -> TInt
      | _ -> failwith "type error in Plus"
    end
  | Eq(e1, e2) ->
		begin
			match (tcheck2 te e1, tcheck2 te e2) with
				(TInt,TInt) -> TBool
			| (TBool,TBool) -> TBool
			| _ -> failwith "type error in Eq"
		end
  | If(e1,e2,e3) -> 
      begin
        match (tcheck2 te e1, tcheck2 te e2, tcheck2 te e3) with
          (TBool,TInt,TInt) -> TInt
        | (TBool,TBool,TBool) -> TBool
        | _ -> failwith "type error in IF"
      end
  | _ -> failwith "unknown expression"

(* tcheck1 : exp -> ty *)
let rec tcheck1 e =
  match e with
  | IntLit(_)    -> TInt
  | BoolLit(_)   -> TBool
  | Plus(e1,e2)  -> 
      begin
        match (tcheck1 e1, tcheck1 e2) with
          (TInt,TInt) -> TInt
        | _ -> failwith "type error in Plus"
      end
  | Eq(e1, e2) ->
		begin
			match (tcheck1 e1, tcheck1 e2) with
				(TInt,TInt) -> TBool
			| (TBool,TBool) -> TBool
			| _ -> failwith "type error in Eq"
		end
  | If(e1,e2,e3) -> 
      begin
        match (tcheck1 e1, tcheck1 e2, tcheck1 e3) with
          (TBool,TInt,TInt) -> TInt
        | (TBool,TBool,TBool) -> TBool
        | _ -> failwith "type error in IF"
      end
  | _ -> failwith "unknown expression"

let rec eval6 e env =
  let binop f e1 e2 env =
    match (eval6 e1 env, eval6 e2 env) with
    | (IntVal(n1),IntVal(n2)) -> IntVal(f n1 n2)
    | _ -> failwith "integer value expected"
  in 
  match e with
  | Var(x)       -> lookup x env 
  | IntLit(n)    -> IntVal(n)
  | Plus(e1,e2)  -> binop (+) e1 e2 env
  | Minus(e1,e2)  -> binop (-) e2 e1 env
  | Times(e1,e2) -> binop ( * ) e1 e2 env
  | Eq(e1,e2)    -> BoolVal((eval6 e1 env)=(eval6 e2 env))
  | If(e1,e2,e3) ->
      begin
        match (eval6 e1 env) with
        | BoolVal(true)  -> eval6 e2 env
        | BoolVal(false) -> eval6 e3 env
        | _ -> failwith "wrong value"
      end
  | Let(x,e1,e2) ->
      let env1 = ext env x (eval6 e1 env) 
      in eval6 e2 env1
  | LetRec(f,x,e1,e2) ->
      let env1 = ext env f (RecFunVal (f, x, e1, env))
      in eval6 e2 env1
  | Fun(x,e1) -> FunVal(x, e1, env)
  | App(e1,e2) ->
      let funpart = (eval6 e1 env) in
      let arg = (eval6 e2 env) in
        begin
         match funpart with
         | FunVal(x,body,env1) ->
            let env2 = (ext env1 x arg) in
            eval6 body env2
         | RecFunVal(f,x,body,env1) ->
            let env2 = (ext (ext env1 x arg) f funpart) in
            eval6 body env2
         | _ -> failwith "wrong value in App"
        end
  | _ -> failwith "unknown expression"

let rec eval4 e env =
  let binop f e1 e2 env =
    match (eval4 e1 env, eval4 e2 env) with
    | (IntVal(n1),IntVal(n2)) -> IntVal(f n1 n2)
    | _ -> failwith "integer value expected"
  in 
  match e with
  | Var(x)       -> lookup x env 
  | IntLit(n)    -> IntVal(n)
  | BoolLit(b)   -> BoolVal(b)
  | Plus(e2,e1)  -> binop (+) e1 e2 env
  | Times(e2,e1) -> binop ( * ) e1 e2 env
  | Eq(e2,e1)    -> BoolVal((eval4 e1 env)=(eval4 e2 env))
  | NotEq(e2,e1)    -> BoolVal(not((eval4 e1 env)=(eval4 e2 env)))
  | If(e1,e2,e3) ->
      begin
        match (eval4 e1 env) with
        | BoolVal(true)  -> eval4 e2 env
        | BoolVal(false) -> eval4 e3 env
        | _ -> failwith "wrong value"
      end
  | Let(x,e2,e1) ->
      let env1 = ext env x (eval4 e1 env) 
      in eval4 e2 env1
  | Fun(x,e1) -> FunVal(x, e1, env)
  | App(e2,e1) ->
     begin
      match (eval4 e1 env) with
        | FunVal(x,body,env1) ->
            let arg = (eval4 e2 env) 
            in eval4 body (ext env1 x arg)
        | _ -> failwith "function value expected"
     end
  | _ -> failwith "unknown expression"

let rec eval3 e env =
  let binop f e1 e2 env =
    match (eval3 e1 env, eval3 e2 env) with
    | (IntVal(n1),IntVal(n2)) -> IntVal(f n1 n2)
    | _ -> failwith "integer value expected"
  in 
  match e with
  | Var(x)       -> lookup x env 
  | IntLit(n)    -> IntVal(n)
  | Plus(e1,e2)  -> binop (+) e1 e2 env
  | Times(e1,e2) -> binop ( * ) e1 e2 env
  | Eq(e1,e2)    -> BoolVal((eval3 e1 env)=(eval3 e2 env))
  | If(e1,e2,e3) ->
      begin
        match (eval3 e1 env) with
        | BoolVal(true)  -> eval3 e2 env
        | BoolVal(false) -> eval3 e3 env
        | _ -> failwith "wrong value"
      end
  | Let(x,e1,e2) ->
      let env1 = ext env x (eval3 e1 env) 
      in eval3 e2 env1
  | _ -> failwith "unknown expression" *)