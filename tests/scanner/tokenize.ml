(* Need to update based on GRAIL's syntax*)

open Parser

let stringify = function
  (* calculation *)
  | PLUS -> "PLUS"   | MINUS -> "MINUS"
  | TIMES -> "TIMES" | DIVIDE -> "DIVIDE"
  (* separator *)
  | SEMI -> "SEMI" | COMMA -> "COMMA"
  | ASSIGN -> "ASSIGN"         | COLON -> "COLON"
  | DOT -> "DOT"
  (* logical operation *)
  | AND -> "AND"      | OR -> "OR"
  | NOT -> "NOT"      | IF -> "IF"
  | ELSE -> "ELSE"    | FOR -> "FOR"
  | WHILE -> "WHILE" 
  (* comparator *)
  | EQ -> "EQ"          | NEQ -> "NEQ"
  | GT -> "GT"      | GEQ -> "GEQ"
  | LT -> "SMALLER"      | LEQ -> "LEQ"
  (* graph operator *)
  | DASH -> "DASH"            | RARROW -> "RARROW"
  | LARROW -> "LARROW"    
 (* | AT -> "AT"
  | AMPERSAND -> "AMPERSAND"  | SIMILARITY -> "SIMILARITY" *)
  (* identifier *)
  | ID(string) -> "ID"
  (* primary type *)
  | INT -> "INT"          | FLOAT -> "FLOAT"
  | STRING -> "STRING"    | BOOL -> "BOOL"
 (* | NODE -> "NODE"        | GRAPH -> "GRAPH" *)
  | LIST -> "LIST"      (*  | DICT -> "DICT" *)
  | NULL -> "NULL"        | VOID -> "VOID"
  
  (* quote 
  | QUOTE -> "QUOTE" *)

  (* boolean operation *)
  (* bracket *)
  | LBRACKET -> "LBRACKET"           | RBRACKET -> "RBRACKET"
  | LBRACE -> "LBRACE" | RBRACE -> "RBRACE"
  | LPAREN -> "LPAREN" | RPAREN -> "RPAREN"
  (* End-of-File *)
  | EOF -> "EOF"
  (* Literals *)
  | INTLIT(int) -> "INT_LITERAL"
  | FLOATLIT(float) -> "FLOAT_LITERAL"
  | STRINGLIT(string) -> "STRING_LITERAL"
  | BOOLIT(bool) -> "BOOLEAN_LITERAL"
  | RETURN -> "RETURN"

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let rec print_tokens = function
    | EOF -> " "
    | token ->
      print_endline (stringify token);
      print_tokens (Scanner.token lexbuf) in
  print_tokens (Scanner.token lexbuf)
