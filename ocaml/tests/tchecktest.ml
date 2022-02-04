open Main;;
open Tcheck;;

let test code = 
    let parsed = parse code
    in
    tcheck [] parsed
in
let code = [
    "
        []
    "; (**Syntax.Empty *)
    "
        [1]
    "; (**Syntax.TList Syntax.TInt *)
    "
        1 :: [2;3]
    "; (**Syntax.TList Syntax.TInt *)
    "   
        true::[false]
    "; (** Syntax.TList Syntax.TBool *)
    "
        List.hd [1; 0; 0; 1; 0; 0;]
    "; (**Syntax.TInt *)
    "
        List.hd []
    "; (* Exception: Failure "type error in Head". *) 
    "
        List.tl []
    "; (**Exception: Failure "type error in Tail". *) 
    "
        List.tl [1; 0; 0; 1; 0; 0;]
    "; (**Syntax.TList Syntax.TInt *)
    "
        List.tl [[1; 0; 0]; [1; 0; 0];]
    "; (**Syntax.TList (Syntax.TList Syntax.TInt) *)
    "
        [2;4;6] :: [[1;3;5]]
    "; (**Syntax.TList (Syntax.TList Syntax.TInt) *)
    "
        List.tl [     
            [2;4;6];     
            [    
                [
                    [1;3];         
                    [5;7]     
                ];
                [
                    [5;4];
                    [5;6];
                    [2;4]
                ]
            ] 
        ]
    "; (**Exception: Failure "type error in Cons". *) 
] 
(* in List.map test code;; *)