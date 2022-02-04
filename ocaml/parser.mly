// S8螳滄ｨ鍋畑 Parser 2011/10/12

%{
open Syntax
%}

// 繝ｪ繝�Λ繝ｫ
%token <string> VAR  // x, y, abc, ...
%token <int> INT     // 0, 1, 2, ...

// 貍皮ｮ怜ｭ�
%token PLUS     // '+'
%token MINUS    // '-'
%token ASTERISK // '*'
%token SLASH    // '/'
%token EQUAL    // '='
%token LESS     // '<'
%token GREATER  // '>'
%token COLCOL   // "::"
%token NOTEQUAL // "<>"

// 諡ｬ蠑ｧ鬘�
%token LPAREN   // '('
%token RPAREN   // ')'
%token LBRA     // '['
%token RBRA     // ']'

// 蛹ｺ蛻�ｊ險伜捷
%token ARROW    // "->"
%token VBAR     // '|'
%token SEMICOL  // ';'

// 繧ｭ繝ｼ繝ｯ繝ｼ繝�
%token TRUE     // "true"
%token FALSE    // "false"
%token FUN      // "fun"
%token LET      // "let"
%token REC      // "rec"
%token IN       // "in"
%token IF       // "if"
%token THEN     // "then"
%token ELSE     // "else"
%token MATCH    // "match"
%token WITH     // "with"
%token HEAD     // "List.hd"
%token TAIL     // "List.tl"
%token NOTEQUAL
%token REMINDER

// 蛻ｶ蠕｡險伜捷
%token EOF 

// 貍皮ｮ怜ｭ仙━蜈磯��ｽ� (蜆ｪ蜈亥ｺｦ縺ｮ菴弱＞繧ゅ�縺ｻ縺ｩ蜈�)
%nonassoc IN ELSE ARROW WITH
%left VBAR
%left SEMICOL
%left EQUAL GREATER LESS
%right COLCOL
%left PLUS MINUS
%left ASTERISK SLASH
%nonassoc UNARY
// 譛蠕後↓arg_expr縺ｮ荳逡ｪ蟾ｦ縺ｮ繝医�繧ｯ繝ｳ繧剃ｸｦ縺ｹ繧�
%left VAR INT TRUE FALSE LBRA LPAREN

%start main
%type <Syntax.exp> main

%%

// 髢句ｧ玖ｨ伜捷
main:
  | exp EOF
    { $1 }
;

// 繝ｪ繧ｹ繝医Μ繝�Λ繝ｫ
list_inner:
  | exp { Cons($1, Empty) }
  | exp SEMICOL { Cons($1, Empty) }
  | exp SEMICOL list_inner { Cons($1, $3) }

// 髢｢謨ｰ縺ｮ蠑墓焚縺ｫ縺ｪ繧後ｋ蠑�
arg_exp:
  | VAR
    { Var $1 }
    
  | INT
    { IntLit $1 }
    
  | TRUE
    { BoolLit true }
    
  | FALSE
    { BoolLit false }
  
  // 遨ｺ繝ｪ繧ｹ繝�
  | LBRA RBRA { Empty }
  
  | LBRA list_inner RBRA { $2 }
  
  // 諡ｬ蠑ｧ縺ｧ蝗ｲ縺ｾ繧後◆蠑�
  | LPAREN exp RPAREN
    { $2 }
;

// 蠑�
exp:
  | arg_exp
    { $1 }
    
  // 髢｢謨ｰ驕ｩ逕ｨ (e1 e2)
  | exp arg_exp
    { App ($1, $2) }
  
  // 隨ｦ蜿ｷ縺ｮ蜿崎ｻ｢ -e
  | MINUS exp %prec UNARY
    { Minus (IntLit 0, $2) }
  
  // e1 + e2
  | exp PLUS exp
    { Plus ($1, $3) }
  
  // e1 - e2
  | exp MINUS exp
    { Minus ($1, $3) }
  
  // e1 * e2
  | exp ASTERISK exp
    { Times ($1, $3) }
  
  // e1 / e2
  | exp SLASH exp
    { Div ($1, $3) }
    
  // e1 = e2
  | exp EQUAL exp
    { Eq ($1, $3) }
  
  // e1 < e2
  | exp LESS exp
    { Less ($1, $3) }
    
  // e1 > e2
  | exp GREATER exp
    { Greater ($1, $3) }
    
  // e1 :: e2
  | exp COLCOL exp
    { Cons ($1, $3) }

  // e1 != e2
  | exp NOTEQUAL exp 
    { NotEq ($1, $3) }
  
  // e1 % e2
  | exp REMINDER exp
    { Reminder ($1, $3)}
    
  // List.hd e
  | HEAD arg_exp
    { Head $2 }
    
  // List.tl e
  | TAIL arg_exp
    { Tail $2 }
  
  // fun x -> e
  | FUN VAR ARROW exp
    { Fun ($2, $4) }
  
  // let x = e1 in e2
  | LET VAR EQUAL exp IN exp
    { Let ($2, $4, $6) }
  
  // let rec f x = e1 in e2
  | LET REC VAR VAR EQUAL exp IN exp
    { LetRec ($3, $4, $6, $8) }
  
  // if e1 then e2 else e3
  | IF exp THEN exp ELSE exp
    { If ($2, $4, $6) }
  
  // match e with ...
  | MATCH exp WITH cases_rev
    { Match ($2, List.rev $4) }
  
  | error
    { 
      let message =
        Printf.sprintf 
          "parse error near characters %d-%d"
          (Parsing.symbol_start ())
	        (Parsing.symbol_end ())
	    in
	    failwith message
	  }
;

// match譁��case縺ｮ蛻�
// 豕ｨ: yacc縺ｧ縺ｯ蟾ｦ蜀榊ｸｰ縺ｮ縺ｻ縺�′繧ｹ繧ｿ繝�け豸郁ｲｻ驥上′蟆代↑縺��
cases_rev:
  | pattern ARROW exp
    { [($1, $3)] }
    
  | cases_rev VBAR pattern ARROW exp
    { ($3, $5) :: $1 }
;

// 繝代ち繝ｼ繝ｳ
pattern:
  | VAR
    { Var $1 }
    
  | INT
    { IntLit $1 }
    
  | TRUE
    { BoolLit true }
    
  | FALSE
    { BoolLit false }
    
  | LBRA RBRA
    { Empty }
    
  | pattern COLCOL pattern
    { Cons ($1, $3) }
;
