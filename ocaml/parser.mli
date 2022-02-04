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

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Syntax.exp
