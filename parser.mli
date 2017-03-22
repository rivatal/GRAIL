type token =
  | SEMI
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | COMMA
  | FUN
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
  | TIMES
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
  | FPLUS
  | FMINUS
  | FTIMES
  | FDIVIDE
  | ADD
  | EADD
  | PLUSEQ
  | FPLUSEQ
  | ADDEQ
  | EADDEQ
  | COPY
  | INTLIT of (int)
  | CHARLIT of (char)
  | DOUBLELIT of (float)
  | STRINGLIT of (string)
  | ID of (string)
  | EOF

val stmt_list :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.stmt_list
