(* S8螳滄ｨ鍋畑 Lexer 2011/10/12 *)

{
open Parser
}

let space = [' ' '\t' '\n' '\r']
let digit = ['0'-'9']
let alpha = ['A'-'Z' 'a'-'z' '_']
let alnum = digit | alpha | '\''

rule token = parse
  (* 謨ｴ謨ｰ螳壽焚 *)
  | digit+
    { let str = Lexing.lexeme lexbuf in
      INT (int_of_string str) }
  
  (* 貍皮ｮ怜ｭ� *)
  | '+'       { PLUS }
  | '-'       { MINUS }
  | '*'       { ASTERISK }
  | '/'       { SLASH }
  | '='       { EQUAL }
  | '<'       { LESS }
  | '>'       { GREATER }
  | ';'       { SEMICOL }
  | "::"      { COLCOL }
  | "!="      { NOTEQUAL }
  | "%"       { REMINDER }

  (* 諡ｬ蠑ｧ鬘� *)
  | '('       { LPAREN }
  | ')'       { RPAREN }
  | '['       { LBRA }
  | ']'       { RBRA }
  
  (* 蛹ｺ蛻�ｊ險伜捷 *)
  | "->"      { ARROW }
  | '|'       { VBAR }
  
  (* 繧ｭ繝ｼ繝ｯ繝ｼ繝� *)
  | "true"    { TRUE }
  | "false"   { FALSE }
  | "fun"     { FUN }
  | "let"     { LET }
  | "rec"     { REC }
  | "in"      { IN }
  | "if"      { IF }
  | "then"    { THEN }
  | "else"    { ELSE }
  | "match"   { MATCH }
  | "with"    { WITH }
  | "List.hd" { HEAD }
  | "List.tl" { TAIL }
  | "mod"     { REMINDER }

  (* 螟画焚 *)
  | alpha alnum*
    { VAR (Lexing.lexeme lexbuf) }
  
  (* 蛻ｶ蠕｡險伜捷 *)
  | eof       { EOF }

  (* 繧ｹ繝壹�繧ｹ繧定ｪｭ縺ｿ鬟帙�縺� *)
  | space+    { token lexbuf }

  | _
    {
      let message = Printf.sprintf
        "unknown token %s near characters %d-%d"
        (Lexing.lexeme lexbuf)
        (Lexing.lexeme_start lexbuf)
        (Lexing.lexeme_end lexbuf)
      in
      failwith message
    }
