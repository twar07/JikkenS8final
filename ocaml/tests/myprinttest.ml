open Myprint;;
open Compile;;
open Interpret;;
open Evaluation;;
open Main;;

let prs e = parse e;;
let comp e = compile (prs e) [];;

let print_exp_test e =
    print_exp (prs e);;

let print_instr_test e =
    let code = comp e
    in
    List.iter print_instr code;;

let print_instrlist_test e =
    print_instrlist (comp e) ;;

let code = [
    "
        let rec factorial n = 
            if n<1 then 0
            else if n=1 then 1 
            else n * (factorial (n-1)) 
        in factorial 5
    "; 
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
    ";
];;

List.map prs code;;
List.iter print_exp_test code;;

(* 
    1.
        REPL
        Syntax.LetRec ("factorial", "n",
            Syntax.If (Syntax.Less (Syntax.Var "n", Syntax.IntLit 1), Syntax.IntLit 0,
            Syntax.If (Syntax.Eq (Syntax.Var "n", Syntax.IntLit 1), Syntax.IntLit 1,
                Syntax.Times (Syntax.Var "n",
                Syntax.App (Syntax.Var "factorial",
                Syntax.Minus (Syntax.Var "n", Syntax.IntLit 1))))),
            Syntax.App (Syntax.Var "factorial", Syntax.IntLit 5))
        
        print_exp
        LetRec("factorial", "n", If(Less(Var "n", IntLit 1), IntLit 0, If(Eq(Var "n", IntLit 1), IntLit 1, Times(Var "n", App(Var "factorial", Minus(Var "n", IntLit 1))))), App(Var "factorial", IntLit 5))

    2.
        REPL
        Syntax.Let ("v", Syntax.IntLit 2,
            Syntax.LetRec ("f", "n",
            Syntax.If (Syntax.Less (Syntax.Var "n", Syntax.IntLit 1), Syntax.IntLit 0,
                Syntax.If (Syntax.Eq (Syntax.Var "n", Syntax.IntLit 1), Syntax.Var "v",
                Syntax.If
                (Syntax.Eq (Syntax.Reminder (Syntax.Var "n", Syntax.IntLit 2),
                    Syntax.IntLit 1),
                Syntax.Times (Syntax.Var "v",
                Syntax.App (Syntax.Var "f",
                    Syntax.Minus (Syntax.Var "n", Syntax.IntLit 1))),
                Syntax.Let ("v2",
                Syntax.App (Syntax.Var "f",
                    Syntax.Div (Syntax.Var "n", Syntax.IntLit 2)),
                Syntax.Times (Syntax.Var "v2", Syntax.Var "v2"))))),
            Syntax.App (Syntax.Var "f", Syntax.IntLit 20)))

        print_exp
        Let("v", IntLit 2, LetRec("f", "n", If(Less(Var "n", IntLit 1), IntLit 0, If(Eq(Var "n", IntLit 1), Var "v", If(Eq(Reminder(Var "n", IntLit 2), IntLit 1), Times(Var "v", App(Var "f", Minus(Var "n", IntLit 1))), Let("v2", App(Var "f", Div(Var "n", IntLit 2)), Times(Var "v2", Var "v2"))))), App(Var "f", IntLit 20)))
        
*)



List.map comp code;;
List.iter print_instr_test code;;
List.iter print_instrlist_test code;;

(* 
    1.
        REPL
        [Syntax.CAM_Closure
            [Syntax.CAM_Ldi 1; Syntax.CAM_Access 0; Syntax.CAM_Less;
                Syntax.CAM_Test ([Syntax.CAM_Ldi 0],
                [Syntax.CAM_Ldi 1; Syntax.CAM_Access 0; Syntax.CAM_Eq;
                Syntax.CAM_Test ([Syntax.CAM_Ldi 1],
                [Syntax.CAM_Ldi 1; Syntax.CAM_Access 0; Syntax.CAM_Minus;
                    Syntax.CAM_Access 1; Syntax.CAM_Apply; Syntax.CAM_Access 0;
                    Syntax.CAM_Times])]);
                Syntax.CAM_Return];
            Syntax.CAM_Let; Syntax.CAM_Ldi 5; Syntax.CAM_Access 0; Syntax.CAM_Apply;
            Syntax.CAM_EndLet]

        print_instr
        CAM_Closure[CAM_Ldi 1; CAM_Access 0; CAM_Less; CAM_Test(CAM_Ldi 0;, CAM_Ldi 1; CAM_Access 0; CAM_Eq; CAM_Test(CAM_Ldi 1;, CAM_Ldi 1; CAM_Access 0; CAM_Minus; CAM_Access 1; CAM_Apply; CAM_Access 0; CAM_Times;);); CAM_Return;]
        CAM_Let
        CAM_Ldi 5
        CAM_Access 0
        CAM_Apply
        CAM_Endlet

        print_instrlist
        [CAM_Closure[CAM_Ldi 1; CAM_Access 0; CAM_Less; CAM_Test(CAM_Ldi 0;, CAM_Ldi 1; CAM_Access 0; CAM_Eq; CAM_Test(CAM_Ldi 1;, CAM_Ldi 1; CAM_Access 0; CAM_Minus; CAM_Access 1; CAM_Apply; CAM_Access 0; CAM_Times;);); CAM_Return;]; CAM_Let; CAM_Ldi 5; CAM_Access 0; CAM_Apply; CAM_Endlet;]

    2.
        REPL
        [Syntax.CAM_Ldi 2; Syntax.CAM_Let;
            Syntax.CAM_Closure
            [Syntax.CAM_Ldi 1; Syntax.CAM_Access 0; Syntax.CAM_Less;
                Syntax.CAM_Test ([Syntax.CAM_Ldi 0],
                [Syntax.CAM_Ldi 1; Syntax.CAM_Access 0; Syntax.CAM_Eq;
                Syntax.CAM_Test ([Syntax.CAM_Access 2],
                [Syntax.CAM_Ldi 1; Syntax.CAM_Ldi 2; Syntax.CAM_Access 0;
                    Syntax.CAM_Reminder; Syntax.CAM_Eq;
                    Syntax.CAM_Test
                    ([Syntax.CAM_Ldi 1; Syntax.CAM_Access 0; Syntax.CAM_Minus;
                    Syntax.CAM_Access 1; Syntax.CAM_Apply; Syntax.CAM_Access 2;
                    Syntax.CAM_Times],
                    [Syntax.CAM_Ldi 2; Syntax.CAM_Access 0; Syntax.CAM_Div;
                    Syntax.CAM_Access 1; Syntax.CAM_Apply; Syntax.CAM_Let;
                    Syntax.CAM_Access 0; Syntax.CAM_Access 0; Syntax.CAM_Times;
                    Syntax.CAM_EndLet])])]);
                Syntax.CAM_Return];
            Syntax.CAM_Let; Syntax.CAM_Ldi 20; Syntax.CAM_Access 0; Syntax.CAM_Apply;
            Syntax.CAM_EndLet; Syntax.CAM_EndLet]

        print_instr
        CAM_Ldi 2
        CAM_Let
        CAM_Closure[CAM_Ldi 1; CAM_Access 0; CAM_Less; CAM_Test(CAM_Ldi 0;, CAM_Ldi 1; CAM_Access 0; CAM_Eq; CAM_Test(CAM_Access 2;, CAM_Ldi 1; CAM_Ldi 2; CAM_Access 0; CAM_Reminder; CAM_Eq; CAM_Test(CAM_Ldi 1; CAM_Access 0; CAM_Minus; CAM_Access 1; CAM_Apply; CAM_Access 2; CAM_Times;, CAM_Ldi 2; CAM_Access 0; CAM_Div; CAM_Access 1; CAM_Apply; CAM_Let; CAM_Access 0; CAM_Access 0; CAM_Times; CAM_Endlet;););); CAM_Return;]
        CAM_Let
        CAM_Ldi 20
        CAM_Access 0
        CAM_Apply
        CAM_Endlet
        CAM_Endlet

        print_instrlist
        [CAM_Ldi 2; CAM_Let; CAM_Closure[CAM_Ldi 1; CAM_Access 0; CAM_Less; CAM_Test(CAM_Ldi 0;, CAM_Ldi 1; CAM_Access 0; CAM_Eq; CAM_Test(CAM_Access 2;, CAM_Ldi 1; CAM_Ldi 2; CAM_Access 0; CAM_Reminder; CAM_Eq; CAM_Test(CAM_Ldi 1; CAM_Access 0; CAM_Minus; CAM_Access 1; CAM_Apply; CAM_Access 2; CAM_Times;, CAM_Ldi 2; CAM_Access 0; CAM_Div; CAM_Access 1; CAM_Apply; CAM_Let; CAM_Access 0; CAM_Access 0; CAM_Times; CAM_Endlet;););); CAM_Return;]; CAM_Let; CAM_Ldi 20; CAM_Access 0; CAM_Apply; CAM_Endlet; CAM_Endlet;]

 *)