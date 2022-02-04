open Syntax;;

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
                        interpret 
                            cc 
                            (v::CAM_ClosVal(cc,ee)::ee) 
                            (CAM_ClosVal(c,e)::s)
                    | _ -> failwith "error in Apply"
                end
            | CAM_Return    -> 
                begin
                    match s with
                    | v::CAM_ClosVal(cc,ee)::s ->
                        interpret 
                            cc ee (v::s)
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
                    | CAM_IntVal(n1)::CAM_IntVal(n2)::s -> 
                        interpret 
                            c e (CAM_IntVal(n1+n2)::s)
                    | _ -> failwith "error in Add"
                end
            | CAM_Eq    -> 
                begin
                    match s with
                    | n1::n2::s -> 
                        begin
                            match (n1, n2) with
                            | CAM_IntVal(v1), CAM_IntVal(v2) ->
                                interpret 
                                    c e (CAM_BoolVal(v1=v2)::s)
                            | CAM_BoolVal(b1), CAM_BoolVal(b2) ->
                                interpret 
                                    c e (CAM_BoolVal(b1=b2)::s)
                            | _ -> failwith "error in Eq"
                        end
                    | _ -> failwith "error in Eq"
                end
            | CAM_NotEq    -> 
                begin
                    match s with
                    | n1::n2::s -> 
                        begin
                            match (n1, n2) with
                            | CAM_IntVal(v1), CAM_IntVal(v2) ->
                                interpret 
                                    c e (CAM_BoolVal(v1!=v2)::s)
                            | CAM_BoolVal(b1), CAM_BoolVal(b2) ->
                                interpret 
                                    c e (CAM_BoolVal(b1!=b2)::s)
                            | _ -> failwith "error in NotEq"
                        end
                    | _ -> failwith "error in NotEq"
                end
            | CAM_Greater    -> 
                begin
                    match s with
                    | n1::n2::s -> 
                        begin
                            match (n1, n2) with
                            | CAM_IntVal(v1), CAM_IntVal(v2) ->
                                interpret 
                                    c e (CAM_BoolVal(v1>v2)::s)
                            | CAM_BoolVal(b1), CAM_BoolVal(b2) ->
                                interpret 
                                    c e (CAM_BoolVal(b1>b2)::s)
                            | _ -> failwith "error in Greater"
                        end
                    | _ -> failwith "error in Greater"
                end
            | CAM_Less   ->
                begin
                    match s with
                    | n1::n2::s -> 
                        begin
                            match (n1, n2) with
                            | CAM_IntVal(v1), CAM_IntVal(v2) ->
                                interpret 
                                    c e (CAM_BoolVal(v1<v2)::s)
                            | CAM_BoolVal(b1), CAM_BoolVal(b2) ->
                                interpret 
                                    c e (CAM_BoolVal(b1<b2)::s)
                            | _ -> failwith "error in Less"
                        end
                    | _ -> failwith "error in Less"
                end
            | CAM_Minus   -> 
                begin
                    match s with
                    | CAM_IntVal(n1)::CAM_IntVal(n2)::s -> 
                        interpret 
                            c e (CAM_IntVal(n1-n2)::s)
                    | _ -> failwith "error in Minus"
                end
            | CAM_Times     ->
                begin
                    match s with
                    | CAM_IntVal(n1)::CAM_IntVal(n2)::s -> 
                        interpret c e (CAM_IntVal(n1*n2)::s)
                    | _ -> failwith "error in Times"
                end
            | CAM_Div     ->
                begin
                    match s with
                    | CAM_IntVal(n1)::CAM_IntVal(n2)::s -> 
                        interpret c e (CAM_IntVal(n1/n2)::s)
                    | _ -> failwith "error in Div"
                end
            | CAM_Reminder  ->
                begin
                    match s with
                    | CAM_IntVal(n1)::CAM_IntVal(n2)::s -> 
                        interpret c e (CAM_IntVal(n1 mod n2)::s)
                    | _ -> failwith "error in Reminder"
                end
        end
    | []    -> 
        match s with
        | hd::tl -> hd
        | [] -> failwith "code expected"