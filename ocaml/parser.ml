type token =
  | VAR of (string)
  | INT of (int)
  | PLUS
  | MINUS
  | ASTERISK
  | SLASH
  | EQUAL
  | LESS
  | GREATER
  | COLCOL
  | NOTEQUAL
  | LPAREN
  | RPAREN
  | LBRA
  | RBRA
  | ARROW
  | VBAR
  | SEMICOL
  | TRUE
  | FALSE
  | FUN
  | LET
  | REC
  | IN
  | IF
  | THEN
  | ELSE
  | MATCH
  | WITH
  | HEAD
  | TAIL
  | REMINDER
  | EOF

open Parsing;;
let _ = parse_error;;
# 4 "parser.mly"
open Syntax
# 41 "parser.ml"
let yytransl_const = [|
  259 (* PLUS *);
  260 (* MINUS *);
  261 (* ASTERISK *);
  262 (* SLASH *);
  263 (* EQUAL *);
  264 (* LESS *);
  265 (* GREATER *);
  266 (* COLCOL *);
  267 (* NOTEQUAL *);
  268 (* LPAREN *);
  269 (* RPAREN *);
  270 (* LBRA *);
  271 (* RBRA *);
  272 (* ARROW *);
  273 (* VBAR *);
  274 (* SEMICOL *);
  275 (* TRUE *);
  276 (* FALSE *);
  277 (* FUN *);
  278 (* LET *);
  279 (* REC *);
  280 (* IN *);
  281 (* IF *);
  282 (* THEN *);
  283 (* ELSE *);
  284 (* MATCH *);
  285 (* WITH *);
  286 (* HEAD *);
  287 (* TAIL *);
  288 (* REMINDER *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  257 (* VAR *);
  258 (* INT *);
    0|]

let yylhs = "\255\255\
\001\000\003\000\003\000\003\000\004\000\004\000\004\000\004\000\
\004\000\004\000\004\000\002\000\002\000\002\000\002\000\002\000\
\002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\005\000\005\000\006\000\006\000\006\000\006\000\006\000\006\000\
\000\000"

let yylen = "\002\000\
\002\000\001\000\002\000\003\000\001\000\001\000\001\000\001\000\
\002\000\003\000\003\000\001\000\002\000\002\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\002\000\002\000\004\000\006\000\008\000\006\000\004\000\001\000\
\003\000\005\000\001\000\001\000\001\000\001\000\002\000\003\000\
\002\000"

let yydefred = "\000\000\
\000\000\000\000\032\000\005\000\006\000\000\000\000\000\000\000\
\007\000\008\000\000\000\000\000\000\000\000\000\000\000\000\000\
\041\000\000\000\012\000\000\000\000\000\009\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\025\000\026\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\001\000\013\000\011\000\000\000\010\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\004\000\000\000\000\000\
\000\000\000\000\035\000\036\000\000\000\037\000\038\000\000\000\
\000\000\000\000\000\000\000\000\039\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000"

let yydgoto = "\002\000\
\017\000\023\000\024\000\019\000\072\000\073\000"

let yysindex = "\255\255\
\092\002\000\000\000\000\000\000\000\000\092\002\092\002\060\002\
\000\000\000\000\004\255\001\255\092\002\092\002\046\255\046\255\
\000\000\033\000\000\000\137\002\144\255\000\000\182\255\251\254\
\011\255\013\255\027\255\215\255\167\001\000\000\000\000\092\002\
\092\002\092\002\092\002\092\002\092\002\092\002\092\002\092\002\
\092\002\000\000\000\000\000\000\092\002\000\000\092\002\092\002\
\028\255\092\002\048\255\002\255\002\255\137\002\137\002\123\002\
\123\002\123\002\123\002\039\002\039\002\000\000\039\002\199\001\
\023\255\231\001\000\000\000\000\016\255\000\000\000\000\015\255\
\255\254\092\002\092\002\092\002\000\000\048\255\048\255\092\002\
\039\002\007\002\039\002\007\255\025\255\039\002\092\002\092\002\
\039\002\039\002"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\051\000\000\000\000\000\021\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\039\255\000\000\000\000\000\000\
\000\000\000\000\000\000\132\000\159\000\078\000\105\000\182\000\
\205\000\228\000\251\000\017\001\032\001\000\000\049\001\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\001\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\067\001\000\000\089\001\000\000\040\255\106\001\000\000\000\000\
\121\001\138\001"

let yygindex = "\000\000\
\000\000\005\000\012\000\054\000\000\000\203\255"

let yytablesize = 937
let yytable = "\001\000\
\031\000\026\000\004\000\005\000\025\000\018\000\034\000\035\000\
\079\000\046\000\020\000\021\000\040\000\007\000\080\000\008\000\
\079\000\028\000\029\000\048\000\009\000\010\000\088\000\027\000\
\084\000\085\000\047\000\049\000\065\000\075\000\077\000\078\000\
\042\000\041\000\079\000\002\000\052\000\053\000\054\000\055\000\
\056\000\057\000\058\000\059\000\060\000\061\000\004\000\005\000\
\067\000\068\000\014\000\063\000\064\000\003\000\066\000\040\000\
\062\000\007\000\000\000\008\000\000\000\069\000\000\000\000\000\
\009\000\010\000\070\000\071\000\030\000\031\000\000\000\043\000\
\000\000\043\000\043\000\000\000\043\000\017\000\081\000\082\000\
\083\000\043\000\043\000\000\000\086\000\000\000\000\000\000\000\
\000\000\000\000\000\000\089\000\090\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\018\000\043\000\043\000\043\000\043\000\043\000\043\000\043\000\
\043\000\043\000\043\000\000\000\043\000\043\000\000\000\043\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\015\000\000\000\000\000\043\000\043\000\
\043\000\000\000\000\000\043\000\000\000\000\000\043\000\043\000\
\004\000\005\000\032\000\033\000\034\000\035\000\036\000\037\000\
\038\000\039\000\040\000\007\000\044\000\008\000\016\000\000\000\
\000\000\000\000\009\000\010\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\041\000\
\000\000\000\000\000\000\000\000\000\000\019\000\004\000\005\000\
\032\000\033\000\034\000\035\000\036\000\037\000\038\000\039\000\
\040\000\007\000\000\000\008\000\000\000\000\000\000\000\045\000\
\009\000\010\000\000\000\000\000\020\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\041\000\000\000\004\000\
\005\000\032\000\033\000\034\000\035\000\036\000\037\000\038\000\
\039\000\040\000\007\000\021\000\008\000\000\000\000\000\000\000\
\000\000\009\000\010\000\000\000\000\000\000\000\000\000\000\000\
\050\000\000\000\000\000\000\000\000\000\000\000\041\000\000\000\
\000\000\000\000\022\000\000\000\000\000\000\000\000\000\000\000\
\000\000\031\000\031\000\031\000\031\000\031\000\031\000\031\000\
\031\000\031\000\031\000\031\000\031\000\031\000\031\000\031\000\
\023\000\000\000\031\000\031\000\031\000\000\000\000\000\000\000\
\031\000\000\000\031\000\031\000\000\000\031\000\000\000\024\000\
\031\000\004\000\005\000\032\000\033\000\034\000\035\000\036\000\
\037\000\038\000\039\000\040\000\007\000\000\000\008\000\000\000\
\027\000\000\000\000\000\009\000\010\000\014\000\014\000\014\000\
\014\000\014\000\014\000\014\000\014\000\000\000\000\000\014\000\
\041\000\014\000\028\000\014\000\014\000\000\000\000\000\000\000\
\000\000\000\000\014\000\000\000\014\000\014\000\000\000\014\000\
\017\000\017\000\017\000\017\000\017\000\017\000\017\000\017\000\
\030\000\000\000\017\000\000\000\017\000\000\000\017\000\017\000\
\000\000\000\000\000\000\000\000\000\000\017\000\000\000\017\000\
\017\000\033\000\017\000\018\000\018\000\018\000\018\000\018\000\
\018\000\018\000\018\000\000\000\000\000\018\000\000\000\018\000\
\029\000\018\000\018\000\000\000\000\000\000\000\000\000\000\000\
\018\000\000\000\018\000\018\000\000\000\018\000\015\000\015\000\
\000\000\034\000\015\000\015\000\015\000\015\000\000\000\000\000\
\015\000\000\000\015\000\000\000\015\000\015\000\000\000\000\000\
\000\000\000\000\000\000\015\000\000\000\015\000\015\000\000\000\
\015\000\016\000\016\000\000\000\000\000\016\000\016\000\016\000\
\016\000\000\000\000\000\016\000\000\000\016\000\000\000\016\000\
\016\000\000\000\000\000\000\000\000\000\000\000\016\000\000\000\
\016\000\016\000\000\000\016\000\019\000\019\000\019\000\000\000\
\000\000\000\000\019\000\000\000\019\000\000\000\019\000\019\000\
\000\000\000\000\000\000\000\000\000\000\019\000\000\000\019\000\
\019\000\000\000\019\000\020\000\020\000\020\000\000\000\000\000\
\000\000\020\000\000\000\020\000\000\000\020\000\020\000\000\000\
\000\000\000\000\000\000\000\000\020\000\000\000\020\000\020\000\
\000\000\020\000\021\000\021\000\021\000\000\000\000\000\000\000\
\021\000\000\000\021\000\000\000\021\000\021\000\000\000\000\000\
\000\000\000\000\000\000\021\000\000\000\021\000\021\000\000\000\
\021\000\022\000\022\000\022\000\000\000\000\000\000\000\022\000\
\000\000\022\000\000\000\022\000\022\000\000\000\000\000\000\000\
\000\000\000\000\022\000\000\000\022\000\022\000\000\000\022\000\
\000\000\000\000\000\000\000\000\000\000\023\000\000\000\023\000\
\000\000\023\000\023\000\000\000\000\000\000\000\000\000\000\000\
\023\000\000\000\023\000\023\000\024\000\023\000\024\000\000\000\
\024\000\024\000\000\000\000\000\000\000\000\000\000\000\024\000\
\000\000\024\000\024\000\000\000\024\000\027\000\000\000\027\000\
\000\000\027\000\027\000\000\000\000\000\000\000\000\000\000\000\
\027\000\000\000\027\000\027\000\000\000\027\000\000\000\028\000\
\000\000\028\000\000\000\028\000\028\000\000\000\000\000\000\000\
\000\000\000\000\028\000\000\000\028\000\028\000\000\000\028\000\
\000\000\000\000\000\000\000\000\000\000\030\000\000\000\030\000\
\000\000\030\000\030\000\000\000\000\000\000\000\000\000\000\000\
\030\000\000\000\030\000\030\000\000\000\030\000\033\000\000\000\
\033\000\000\000\033\000\033\000\000\000\000\000\000\000\000\000\
\000\000\033\000\000\000\033\000\033\000\029\000\033\000\029\000\
\000\000\029\000\029\000\000\000\000\000\000\000\000\000\000\000\
\029\000\000\000\029\000\029\000\000\000\029\000\034\000\000\000\
\034\000\000\000\034\000\034\000\000\000\000\000\000\000\000\000\
\000\000\034\000\000\000\034\000\034\000\000\000\034\000\004\000\
\005\000\032\000\033\000\034\000\035\000\036\000\037\000\038\000\
\039\000\040\000\007\000\000\000\008\000\000\000\000\000\000\000\
\000\000\009\000\010\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\051\000\000\000\000\000\041\000\004\000\
\005\000\032\000\033\000\034\000\035\000\036\000\037\000\038\000\
\039\000\040\000\007\000\000\000\008\000\000\000\000\000\000\000\
\000\000\009\000\010\000\000\000\000\000\000\000\074\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\041\000\004\000\
\005\000\032\000\033\000\034\000\035\000\036\000\037\000\038\000\
\039\000\040\000\007\000\000\000\008\000\000\000\000\000\000\000\
\000\000\009\000\010\000\000\000\000\000\000\000\000\000\000\000\
\000\000\076\000\000\000\000\000\000\000\000\000\041\000\004\000\
\005\000\032\000\033\000\034\000\035\000\036\000\037\000\038\000\
\039\000\040\000\007\000\000\000\008\000\000\000\000\000\000\000\
\000\000\009\000\010\000\000\000\000\000\000\000\087\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\041\000\004\000\
\005\000\032\000\033\000\034\000\035\000\036\000\037\000\038\000\
\039\000\040\000\007\000\000\000\008\000\000\000\000\000\000\000\
\000\000\009\000\010\000\003\000\004\000\005\000\000\000\006\000\
\000\000\000\000\000\000\000\000\000\000\000\000\041\000\007\000\
\000\000\008\000\022\000\000\000\000\000\000\000\009\000\010\000\
\011\000\012\000\000\000\000\000\013\000\000\000\000\000\014\000\
\000\000\015\000\016\000\003\000\004\000\005\000\000\000\006\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\007\000\
\000\000\008\000\000\000\000\000\000\000\000\000\009\000\010\000\
\011\000\012\000\000\000\000\000\013\000\000\000\000\000\014\000\
\000\000\015\000\016\000\004\000\005\000\032\000\033\000\034\000\
\035\000\000\000\000\000\000\000\039\000\040\000\007\000\000\000\
\008\000\004\000\005\000\000\000\000\000\009\000\010\000\000\000\
\000\000\000\000\000\000\040\000\007\000\000\000\008\000\000\000\
\000\000\000\000\041\000\009\000\010\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\041\000"

let yycheck = "\001\000\
\000\000\001\001\001\001\002\001\001\001\001\000\005\001\006\001\
\010\001\015\001\006\000\007\000\011\001\012\001\016\001\014\001\
\010\001\013\000\014\000\007\001\019\001\020\001\016\001\023\001\
\078\000\079\000\016\001\001\001\001\001\007\001\015\001\017\001\
\000\000\032\001\010\001\015\001\032\000\033\000\034\000\035\000\
\036\000\037\000\038\000\039\000\040\000\041\000\001\001\002\001\
\001\001\002\001\000\000\047\000\048\000\015\001\050\000\016\001\
\045\000\012\001\255\255\014\001\255\255\014\001\255\255\255\255\
\019\001\020\001\019\001\020\001\015\000\016\000\255\255\018\000\
\255\255\020\000\021\000\255\255\023\000\000\000\074\000\075\000\
\076\000\028\000\029\000\255\255\080\000\255\255\255\255\255\255\
\255\255\255\255\255\255\087\000\088\000\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\000\000\052\000\053\000\054\000\055\000\056\000\057\000\058\000\
\059\000\060\000\061\000\255\255\063\000\064\000\255\255\066\000\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\000\000\255\255\255\255\081\000\082\000\
\083\000\255\255\255\255\086\000\255\255\255\255\089\000\090\000\
\001\001\002\001\003\001\004\001\005\001\006\001\007\001\008\001\
\009\001\010\001\011\001\012\001\013\001\014\001\000\000\255\255\
\255\255\255\255\019\001\020\001\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\032\001\
\255\255\255\255\255\255\255\255\255\255\000\000\001\001\002\001\
\003\001\004\001\005\001\006\001\007\001\008\001\009\001\010\001\
\011\001\012\001\255\255\014\001\255\255\255\255\255\255\018\001\
\019\001\020\001\255\255\255\255\000\000\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\032\001\255\255\001\001\
\002\001\003\001\004\001\005\001\006\001\007\001\008\001\009\001\
\010\001\011\001\012\001\000\000\014\001\255\255\255\255\255\255\
\255\255\019\001\020\001\255\255\255\255\255\255\255\255\255\255\
\026\001\255\255\255\255\255\255\255\255\255\255\032\001\255\255\
\255\255\255\255\000\000\255\255\255\255\255\255\255\255\255\255\
\255\255\001\001\002\001\003\001\004\001\005\001\006\001\007\001\
\008\001\009\001\010\001\011\001\012\001\013\001\014\001\015\001\
\000\000\255\255\018\001\019\001\020\001\255\255\255\255\255\255\
\024\001\255\255\026\001\027\001\255\255\029\001\255\255\000\000\
\032\001\001\001\002\001\003\001\004\001\005\001\006\001\007\001\
\008\001\009\001\010\001\011\001\012\001\255\255\014\001\255\255\
\000\000\255\255\255\255\019\001\020\001\003\001\004\001\005\001\
\006\001\007\001\008\001\009\001\010\001\255\255\255\255\013\001\
\032\001\015\001\000\000\017\001\018\001\255\255\255\255\255\255\
\255\255\255\255\024\001\255\255\026\001\027\001\255\255\029\001\
\003\001\004\001\005\001\006\001\007\001\008\001\009\001\010\001\
\000\000\255\255\013\001\255\255\015\001\255\255\017\001\018\001\
\255\255\255\255\255\255\255\255\255\255\024\001\255\255\026\001\
\027\001\000\000\029\001\003\001\004\001\005\001\006\001\007\001\
\008\001\009\001\010\001\255\255\255\255\013\001\255\255\015\001\
\000\000\017\001\018\001\255\255\255\255\255\255\255\255\255\255\
\024\001\255\255\026\001\027\001\255\255\029\001\003\001\004\001\
\255\255\000\000\007\001\008\001\009\001\010\001\255\255\255\255\
\013\001\255\255\015\001\255\255\017\001\018\001\255\255\255\255\
\255\255\255\255\255\255\024\001\255\255\026\001\027\001\255\255\
\029\001\003\001\004\001\255\255\255\255\007\001\008\001\009\001\
\010\001\255\255\255\255\013\001\255\255\015\001\255\255\017\001\
\018\001\255\255\255\255\255\255\255\255\255\255\024\001\255\255\
\026\001\027\001\255\255\029\001\007\001\008\001\009\001\255\255\
\255\255\255\255\013\001\255\255\015\001\255\255\017\001\018\001\
\255\255\255\255\255\255\255\255\255\255\024\001\255\255\026\001\
\027\001\255\255\029\001\007\001\008\001\009\001\255\255\255\255\
\255\255\013\001\255\255\015\001\255\255\017\001\018\001\255\255\
\255\255\255\255\255\255\255\255\024\001\255\255\026\001\027\001\
\255\255\029\001\007\001\008\001\009\001\255\255\255\255\255\255\
\013\001\255\255\015\001\255\255\017\001\018\001\255\255\255\255\
\255\255\255\255\255\255\024\001\255\255\026\001\027\001\255\255\
\029\001\007\001\008\001\009\001\255\255\255\255\255\255\013\001\
\255\255\015\001\255\255\017\001\018\001\255\255\255\255\255\255\
\255\255\255\255\024\001\255\255\026\001\027\001\255\255\029\001\
\255\255\255\255\255\255\255\255\255\255\013\001\255\255\015\001\
\255\255\017\001\018\001\255\255\255\255\255\255\255\255\255\255\
\024\001\255\255\026\001\027\001\013\001\029\001\015\001\255\255\
\017\001\018\001\255\255\255\255\255\255\255\255\255\255\024\001\
\255\255\026\001\027\001\255\255\029\001\013\001\255\255\015\001\
\255\255\017\001\018\001\255\255\255\255\255\255\255\255\255\255\
\024\001\255\255\026\001\027\001\255\255\029\001\255\255\013\001\
\255\255\015\001\255\255\017\001\018\001\255\255\255\255\255\255\
\255\255\255\255\024\001\255\255\026\001\027\001\255\255\029\001\
\255\255\255\255\255\255\255\255\255\255\013\001\255\255\015\001\
\255\255\017\001\018\001\255\255\255\255\255\255\255\255\255\255\
\024\001\255\255\026\001\027\001\255\255\029\001\013\001\255\255\
\015\001\255\255\017\001\018\001\255\255\255\255\255\255\255\255\
\255\255\024\001\255\255\026\001\027\001\013\001\029\001\015\001\
\255\255\017\001\018\001\255\255\255\255\255\255\255\255\255\255\
\024\001\255\255\026\001\027\001\255\255\029\001\013\001\255\255\
\015\001\255\255\017\001\018\001\255\255\255\255\255\255\255\255\
\255\255\024\001\255\255\026\001\027\001\255\255\029\001\001\001\
\002\001\003\001\004\001\005\001\006\001\007\001\008\001\009\001\
\010\001\011\001\012\001\255\255\014\001\255\255\255\255\255\255\
\255\255\019\001\020\001\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\029\001\255\255\255\255\032\001\001\001\
\002\001\003\001\004\001\005\001\006\001\007\001\008\001\009\001\
\010\001\011\001\012\001\255\255\014\001\255\255\255\255\255\255\
\255\255\019\001\020\001\255\255\255\255\255\255\024\001\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\032\001\001\001\
\002\001\003\001\004\001\005\001\006\001\007\001\008\001\009\001\
\010\001\011\001\012\001\255\255\014\001\255\255\255\255\255\255\
\255\255\019\001\020\001\255\255\255\255\255\255\255\255\255\255\
\255\255\027\001\255\255\255\255\255\255\255\255\032\001\001\001\
\002\001\003\001\004\001\005\001\006\001\007\001\008\001\009\001\
\010\001\011\001\012\001\255\255\014\001\255\255\255\255\255\255\
\255\255\019\001\020\001\255\255\255\255\255\255\024\001\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\032\001\001\001\
\002\001\003\001\004\001\005\001\006\001\007\001\008\001\009\001\
\010\001\011\001\012\001\255\255\014\001\255\255\255\255\255\255\
\255\255\019\001\020\001\000\001\001\001\002\001\255\255\004\001\
\255\255\255\255\255\255\255\255\255\255\255\255\032\001\012\001\
\255\255\014\001\015\001\255\255\255\255\255\255\019\001\020\001\
\021\001\022\001\255\255\255\255\025\001\255\255\255\255\028\001\
\255\255\030\001\031\001\000\001\001\001\002\001\255\255\004\001\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\012\001\
\255\255\014\001\255\255\255\255\255\255\255\255\019\001\020\001\
\021\001\022\001\255\255\255\255\025\001\255\255\255\255\028\001\
\255\255\030\001\031\001\001\001\002\001\003\001\004\001\005\001\
\006\001\255\255\255\255\255\255\010\001\011\001\012\001\255\255\
\014\001\001\001\002\001\255\255\255\255\019\001\020\001\255\255\
\255\255\255\255\255\255\011\001\012\001\255\255\014\001\255\255\
\255\255\255\255\032\001\019\001\020\001\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\032\001"

let yynames_const = "\
  PLUS\000\
  MINUS\000\
  ASTERISK\000\
  SLASH\000\
  EQUAL\000\
  LESS\000\
  GREATER\000\
  COLCOL\000\
  NOTEQUAL\000\
  LPAREN\000\
  RPAREN\000\
  LBRA\000\
  RBRA\000\
  ARROW\000\
  VBAR\000\
  SEMICOL\000\
  TRUE\000\
  FALSE\000\
  FUN\000\
  LET\000\
  REC\000\
  IN\000\
  IF\000\
  THEN\000\
  ELSE\000\
  MATCH\000\
  WITH\000\
  HEAD\000\
  TAIL\000\
  REMINDER\000\
  EOF\000\
  "

let yynames_block = "\
  VAR\000\
  INT\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'exp) in
    Obj.repr(
# 73 "parser.mly"
    ( _1 )
# 432 "parser.ml"
               : Syntax.exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 78 "parser.mly"
        ( Cons(_1, Empty) )
# 439 "parser.ml"
               : 'list_inner))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'exp) in
    Obj.repr(
# 79 "parser.mly"
                ( Cons(_1, Empty) )
# 446 "parser.ml"
               : 'list_inner))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'list_inner) in
    Obj.repr(
# 80 "parser.mly"
                           ( Cons(_1, _3) )
# 454 "parser.ml"
               : 'list_inner))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 85 "parser.mly"
    ( Var _1 )
# 461 "parser.ml"
               : 'arg_exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 88 "parser.mly"
    ( IntLit _1 )
# 468 "parser.ml"
               : 'arg_exp))
; (fun __caml_parser_env ->
    Obj.repr(
# 91 "parser.mly"
    ( BoolLit true )
# 474 "parser.ml"
               : 'arg_exp))
; (fun __caml_parser_env ->
    Obj.repr(
# 94 "parser.mly"
    ( BoolLit false )
# 480 "parser.ml"
               : 'arg_exp))
; (fun __caml_parser_env ->
    Obj.repr(
# 97 "parser.mly"
              ( Empty )
# 486 "parser.ml"
               : 'arg_exp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'list_inner) in
    Obj.repr(
# 99 "parser.mly"
                         ( _2 )
# 493 "parser.ml"
               : 'arg_exp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'exp) in
    Obj.repr(
# 103 "parser.mly"
    ( _2 )
# 500 "parser.ml"
               : 'arg_exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'arg_exp) in
    Obj.repr(
# 109 "parser.mly"
    ( _1 )
# 507 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'exp) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'arg_exp) in
    Obj.repr(
# 113 "parser.mly"
    ( App (_1, _2) )
# 515 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 117 "parser.mly"
    ( Minus (IntLit 0, _2) )
# 522 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 121 "parser.mly"
    ( Plus (_1, _3) )
# 530 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 125 "parser.mly"
    ( Minus (_1, _3) )
# 538 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 129 "parser.mly"
    ( Times (_1, _3) )
# 546 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 133 "parser.mly"
    ( Div (_1, _3) )
# 554 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 137 "parser.mly"
    ( Eq (_1, _3) )
# 562 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 141 "parser.mly"
    ( Less (_1, _3) )
# 570 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 145 "parser.mly"
    ( Greater (_1, _3) )
# 578 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 149 "parser.mly"
    ( Cons (_1, _3) )
# 586 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 153 "parser.mly"
    ( NotEq (_1, _3) )
# 594 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 157 "parser.mly"
    ( Reminder (_1, _3))
# 602 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'arg_exp) in
    Obj.repr(
# 161 "parser.mly"
    ( Head _2 )
# 609 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'arg_exp) in
    Obj.repr(
# 165 "parser.mly"
    ( Tail _2 )
# 616 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 169 "parser.mly"
    ( Fun (_2, _4) )
# 624 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 173 "parser.mly"
    ( Let (_2, _4, _6) )
# 633 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 177 "parser.mly"
    ( LetRec (_3, _4, _6, _8) )
# 643 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'exp) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 181 "parser.mly"
    ( If (_2, _4, _6) )
# 652 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'cases_rev) in
    Obj.repr(
# 185 "parser.mly"
    ( Match (_2, List.rev _4) )
# 660 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    Obj.repr(
# 188 "parser.mly"
    ( 
      let message =
        Printf.sprintf 
          "parse error near characters %d-%d"
          (Parsing.symbol_start ())
	        (Parsing.symbol_end ())
	    in
	    failwith message
	  )
# 674 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'pattern) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 203 "parser.mly"
    ( [(_1, _3)] )
# 682 "parser.ml"
               : 'cases_rev))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'cases_rev) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'pattern) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 206 "parser.mly"
    ( (_3, _5) :: _1 )
# 691 "parser.ml"
               : 'cases_rev))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 212 "parser.mly"
    ( Var _1 )
# 698 "parser.ml"
               : 'pattern))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 215 "parser.mly"
    ( IntLit _1 )
# 705 "parser.ml"
               : 'pattern))
; (fun __caml_parser_env ->
    Obj.repr(
# 218 "parser.mly"
    ( BoolLit true )
# 711 "parser.ml"
               : 'pattern))
; (fun __caml_parser_env ->
    Obj.repr(
# 221 "parser.mly"
    ( BoolLit false )
# 717 "parser.ml"
               : 'pattern))
; (fun __caml_parser_env ->
    Obj.repr(
# 224 "parser.mly"
    ( Empty )
# 723 "parser.ml"
               : 'pattern))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'pattern) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'pattern) in
    Obj.repr(
# 227 "parser.mly"
    ( Cons (_1, _3) )
# 731 "parser.ml"
               : 'pattern))
(* Entry main *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let main (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Syntax.exp)