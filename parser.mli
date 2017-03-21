type token =
  | SEMI
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | COMMA
  | PLUS
  | MINUS
  | DIVIDE
  | ASSIGN
  | NOT
  | DOT
  | COLON
  | EQ
  | NEQ
  | LT
  | LEQ
  | GT
  | GEQ
  | TRUE
  | FALSE
  | AND
  | OR
  | RETURN
  | IF
  | ELSE
  | FOR
  | WHILE
  | INT
  | BOOLEAN
  | VOID
  | STAR
  | LBRACKET
  | RBRACKET
  | DASH
  | RARROW
  | LARROW
  | ACCIO
  | BREAK
  | CHAR
  | DOUBLE
  | EDGE
  | EMPTY
  | CONTINUE
  | TO
  | FROM
  | IN
  | NODE
  | RECORD
  | TYPE
  | WITH
  | FREE
  | INTLIT of (int)
  | CHARLIT of (char)
  | DOUBLELIT of (float)
  | STRINGLIT of (string)
  | ID of (string)
  | EOF

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.program
