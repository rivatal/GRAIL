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
| ".+"     { FPLUS }
| '-'      { MINUS }
| ".-"     { FMINUS }
| '*'      { TIMES }
| ".*"     { FTIMES }
| '/'      { DIVIDE }
| "./"     { FDIVIDE }
| "&"      { ADD }
| ".&"     { EADD }
| "+="     { PLUSEQ }
| ".+="    { FPLUSEQ }
| "&="     { ADDEQ }
| ".&="    { EADDEQ }
| '='      { ASSIGN }
| ".="     { COPY }
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
| "accio"  { ACCIO }
| "else"   { ELSE }
| "false"  { FALSE }
| "for"    { FOR }
| "free"   { FREE }
| "from"   { FROM }
| "if"     { IF }
| "in"     { IN }
| "return" { RETURN }
| "true"   { TRUE }
| "type"   { TYPE }
| "while"  { WHILE }
| "with"   { WITH }
| ['0'-'9']+ as lxm { INTLIT(int_of_string lxm) }
| ['0'-'9']*'.'['0'-'9']* as lxm { DOUBLELIT(float_of_string lxm) }
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }
| '''(_ as mychar)''' { CHARLIT(mychar) }
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
| [^ '\\' '"']+ { Buffer.add_string strbuf (Lexing.lexeme lexbuf); str strbuf lexbuf }
| eof { raise (Failure ("Unterminated String")) }
| _ { raise ( Failure("Problem with string")) }
