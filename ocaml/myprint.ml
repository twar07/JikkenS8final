open Syntax;;

let print_exp e = 
  let lparen = "("
  in
  let rparen = ")"
  in
  let rec get_internal e =
    let binop f e1 e2 =
      f ^ lparen ^ (get_internal e1 ) ^ ", " ^ (get_internal e2 ) ^ rparen
    in
    let string_v v = "\"" ^ v ^ "\""
    in
      match e with
    | Var(x)          -> "Var" ^ " " ^ (string_v x)
    | IntLit(n)       -> "IntLit" ^ " " ^ (string_of_int n)
    | BoolLit(b)      -> "BoolLit" ^ " " ^ (string_of_bool b)
    | Plus(e1,e2)     -> binop "Plus" e1 e2 
    | Minus(e1,e2)    -> binop "Minus" e1 e2 
    | Times(e1,e2)    -> binop "Times" e1 e2 
    | Div(e1,e2)      -> binop "Div" e1 e2 
    | Reminder(e1,e2) -> binop "Reminder" e1 e2 
    | Eq(e1,e2)       -> binop "Eq" e1 e2 
    | NotEq(e1,e2)    -> binop "NotEq" e1 e2 
    | Greater(e1,e2)  -> binop "Greater" e1 e2 
    | Less(e1,e2)     -> binop "Less" e1 e2 
    | If(e1,e2,e3)    -> 
      "If" ^ lparen ^ (get_internal e1) ^ ", " ^ (get_internal e2) ^ ", " ^ (get_internal e3) ^ rparen
    | Let(x,e1,e2)    -> 
      "Let" ^ lparen ^ (string_v x) ^ ", " ^ (get_internal e1 ) ^ ", " ^ (get_internal e2 ) ^ rparen
    | LetRec(f,x,e1,e2) -> 
      "LetRec" ^ lparen ^ (string_v f) ^ ", " ^ (string_v x) ^ ", " ^ (get_internal e1) ^ ", " ^ (get_internal e2) ^ rparen
    | Fun(x,e1)       -> 
      "Fun" ^ lparen ^ (string_v x) ^ ", " ^ (get_internal e1) ^ rparen
    | App(e1,e2)      -> 
      "App" ^ lparen ^ (get_internal e1) ^ ", " ^ (get_internal e2) ^ rparen
    | Empty           -> "Empty"
    | Cons(e1,e2)     -> 
      "Cons" ^ lparen ^ (get_internal e1) ^ ", " ^ (get_internal e2) ^ rparen
    | Head(e1)        -> "Head" ^ lparen ^ (get_internal e1) ^ rparen
    | Tail(e1)        -> "Tail" ^ lparen ^ (get_internal e1) ^ rparen
    | _ -> failwith "unknown expression"
  in
  print_endline (get_internal e);;

let print_instr e = 
  let lparen = "("
  in
  let rparen = ")"
  in
  let lbra = "["
  in
  let rbra = "]"
  in 
  let rec get_internal e =
    let rec get_internal_code c =
      match c with
      | cam_code ->
        begin
          match c with
          | hd::tl -> 
            (get_internal hd) 
            ^ begin
              match tl with
              | [] -> ";"
              | _  -> "; "
            end
            ^ (get_internal_code tl)
          | _ -> ""
        end
      | _ -> failwith "code error expected"
    in
    begin 
      match e with
      | CAM_Ldi(n)        -> "CAM_Ldi" ^ " " ^ (string_of_int n)
      | CAM_Ldb(b)        -> "CAM_Ldb" ^ " " ^ (string_of_bool b)
      | CAM_Access(i)     -> "CAM_Access" ^ " " ^ (string_of_int i)
      | CAM_Closure(c1)   -> 
        "CAM_Closure" ^ lbra ^ (get_internal_code c1) ^ rbra
      | CAM_Apply         -> "CAM_Apply"
      | CAM_Return        -> "CAM_Return"
      | CAM_Let           -> "CAM_Let"
      | CAM_EndLet        -> "CAM_Endlet"
      | CAM_Test(c1, c2)  -> 
        "CAM_Test" ^ lparen ^ (get_internal_code c1) ^ ", " ^ (get_internal_code c2) ^ rparen
      | CAM_Add           -> "CAM_Add"
      | CAM_Eq            -> "CAM_Eq"
      | CAM_NotEq         -> "CAM_NotEq"
      | CAM_Greater       -> "CAM_Greater"
      | CAM_Less          -> "CAM_Less"
      | CAM_Minus         -> "CAM_Minus"
      | CAM_Times         -> "CAM_Times"
      | CAM_Div           -> "CAM_Div"
      | CAM_Reminder      -> "CAM_Reminder"
      | _ -> failwith "unknown expression"
    end
  in
  print_endline (get_internal e);;

let print_instrlist e = 
  let lparen = "("
  in
  let rparen = ")"
  in
  let lbra = "["
  in
  let rbra = "]"
  in 
  let rec get_internal_code c =
    let rec get_internal e =
      begin 
        match e with
        | CAM_Ldi(n)        -> "CAM_Ldi" ^ " " ^ (string_of_int n)
        | CAM_Ldb(b)        -> "CAM_Ldb" ^ " " ^ (string_of_bool b)
        | CAM_Access(i)     -> "CAM_Access" ^ " " ^ (string_of_int i)
        | CAM_Closure(c1)   -> 
          "CAM_Closure" ^ lbra ^ (get_internal_code c1) ^ rbra
        | CAM_Apply         -> "CAM_Apply"
        | CAM_Return        -> "CAM_Return"
        | CAM_Let           -> "CAM_Let"
        | CAM_EndLet        -> "CAM_Endlet"
        | CAM_Test(c1, c2)  -> 
          "CAM_Test" ^ lparen ^ (get_internal_code c1) ^ ", " ^ (get_internal_code c2) ^ rparen
        | CAM_Add           -> "CAM_Add"
        | CAM_Eq            -> "CAM_Eq"
        | CAM_NotEq         -> "CAM_NotEq"
        | CAM_Greater       -> "CAM_Greater"
        | CAM_Less          -> "CAM_Less"
        | CAM_Minus         -> "CAM_Minus"
        | CAM_Times         -> "CAM_Times"
        | CAM_Div           -> "CAM_Div"
        | CAM_Reminder      -> "CAM_Reminder"
        | _ -> failwith "unknown expression"
      end
    in
    match c with
    | cam_code ->
      begin
        match c with
        | hd::tl -> 
          (get_internal hd) 
          ^ begin
            match tl with
            | [] -> ";"
            | _  -> "; "
          end
          ^ (get_internal_code tl)
        | _ -> ""
      end
    | _ -> failwith "code error expected"
  in
  print_endline (lbra ^ (get_internal_code e) ^rbra);;
