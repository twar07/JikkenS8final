open Main;;
open Evaluation;;

let run str =
  let parsed = parse str
  in evaluation parsed [];;

let result str = 
  let v =
    match run str with
    | IntVal(n) -> string_of_int n
    | BoolVal(b) -> string_of_bool b
    | _ -> "unknown expression"
  in print_endline v

let bench c = 
    let ed = 1000
    in
    let a = Sys.time () in
    ignore (
        for i=1 to ed do
            run c
        done
    );
    print_float ((Sys.time () -. a) /. 10.); print_endline "[s]"

let code = [
    "
        let rec factorial n = 
            if n<1 then 0
            else if n=1 then 1 
            else n * (factorial (n-1)) 
        in factorial 5
    "; (* 120 *)
    "
        let rec factorial n = 
            if n<1 then 0 
            else if n=1 then 1 
            else n * (factorial (n-1)) 
            in factorial true
    "; (* Exception: Failure "wrong value". *) 
    "
        let rec f x n = 
            if n=1 then x
            else
                if n mod 2=0 then (f (x*x) (n/2)) 
                else x*(f (x*x) (n/2)) 
            in f 2 20
    "; (* Exception: Failure "parse error near characters 22-23". *) 
    "
        let v = 2
        in let rec f n
            = if n<1 then 0 
                else if n=1 then v
                else v*(f (n-1))
        in f 20
    "; (* 1048576 *)
    "
        let v = 2
        in let rec f n = 
            if n<1 then 0 
            else if n=1 then v
            else if (n mod 2)=1 then
                v * f (n-1)
            else
                let v2 = f (n/2)
                in v2 * v2
        in f 20
    "; (* 1048576 *)
    "
        let v = 2
        in let rec f n
            = if n<1 then 0 
                else if n=1 then v
                else if (n mod 2)=false then
                    v * f (n-1)
                else
                    let v2 = f (n/2)
                    in v2 * v2
        in f 20
    "; (* Exception: Failure "wrong value". *) 
];;
(* List.iter result code;; *)

let code = [
    "
        let m = 1000000007
        in
        let rec factorial n = 
            if n<1 then 0 
            else if n=1 then 1 
            else (n * ((factorial (n-1)) mod m) mod m)
            in factorial 10000
    "; (* 1.191755[s] *)
    "
        let m = 1000000007
        in
        let v = 2 mod m
        in let rec f n
            = if n<1 then 0 
                else if n=1 then v
                else (v*((f (n-1)) mod m) mod m)
        in f 10000
    "; (* 1.2292471[s] *)
    "
        let m = 1000000007
        in
        let v = 2 mod m
        in let rec f n = 
            if n<1 then 0 
            else if n=1 then v
            else if (n mod 2)=1 then
                (v * ((f (n-1)) mod m) mod m)
            else
                let v2 = (f (n/2)) mod m
                in (v2 * v2) mod m
        in f 10000
    "; (* 0.0056218[s] *)
];;
(* List.iter bench code;;  *)