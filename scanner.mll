(* Ocamllex scanner for GRAIL *)

{ open Parser }

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
| "/*"     { comment lexbuf }           (* Comments *)
| "//"     { oneline lexbuf }
| '"'      { str (Buffer.create 16) lexbuf }
| '('      { LPAREN }
| ')'      { RPAREN }
| '{'      { LBRACE }
| '}'      { RBRACE }
| '['      { LBRACKET }
| ']'      { RBRACKET }
| ';'      { SEMI }
| ','      { COMMA }
| '.'      { DOT }
| '+'      { PLUS }
| '-'      { MINUS }
| '*'      { STAR }
| '/'      { DIVIDE }
| '='      { ASSIGN }
| "=="     { EQ }
| "!="     { NEQ }
| '<'      { LT }
| "<="     { LEQ }
| ">"      { GT }
| ">="     { GEQ }
| "&&"     { AND }
| "||"     { OR }
| "!"      { NOT }
| "--"     { DASH }
| "->"    { RARROW }
| "<-"     { LARROW }
| ':'      { COLON }
| "accio"  {ACCIO}
| "boolean" { BOOLEAN }
| "break"  { BREAK }
| "char"   { CHAR }
| "double" { DOUBLE }
| "edge"   { EDGE }
| "else"   { ELSE }
| "empty"  { EMPTY }
| "false"  { FALSE }
| "for"    { FOR }
| "free"   { FREE }
| "from"   { FROM }
| "graph"  { GRAPH }
| "if"     { IF }
| "in"     { IN }
| "int"    { INT }
| "node"   { NODE }
| "record" { RECORD }
| "return" { RETURN }
| "true"   { TRUE }
| "type"   { TYPE }
| "void"   { VOID }
| "while"  { WHILE }
| "with"   { WITH }
| ['0'-'9']+ as lxm { INTLIT(int_of_string lxm) }
| ['0'-'9']*'.'['0'-'9']* as lxm { DOUBLELIT(float_of_string) }
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }
| '''(_ as mychar) ''' { CHARLIT(mychar) }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  "*/" { token lexbuf }
| _    { comment lexbuf }

and oneline = parse
  '\n' { token lexbuf }
| _ { oneline lexbuf }

and str strbuf = parse
  '"' { STRINGLIT( Buffer.contents strbuf ) }
| '\\' '"' { Buffer.add_char strbuf '"'; str strbuf lexbuf}
| '\\'  { Buffer.add_char strbuf '\\'; str strbuf lexbuf}
| [^ '\\' '"']+ { Buffer.add_string strbuf (Lexing.lexeme lexbuf); }
| eof { raise (Failure ("Unterminated String")) }
| _ { raise ( Failure("Problem with string")) }
