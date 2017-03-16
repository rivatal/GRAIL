/*Parser for GRAIL*/
%{
open Ast
%}

%token SEMI LPAREN RPAREN LBRACE RBRACE COMMA
%token PLUS MINUS DIVIDE ASSIGN NOT DOT COLON
%token EQ NEQ LT LEQ GT GEQ TRUE FALSE AND OR
%token RETURN IF ELSE FOR WHILE INT BOOLEAN VOID
%token STAR LBRACKET RBRACKET DASH RARROW LARROW
%token ACCIO BREAK CHAR DOUBLE EDGE EMPTY CONTINUE
%token TO FROM IN NODE RECORD TYPE WITH FREE
%token <int> INTLIT
%token <char> CHARLIT
%token <float> DOUBLELIT
%token <string> STRINGLIT
%token <string> ID
%token EOF

%nonassoc NOELSE
%nonassoc ELSE
%right ASSIGN
%left OR
%left AND
%left EQ NEQ
%left LT GT LEQ GEQ IN
%left STAR
%right DOT
%nonassoc NOWITH
%nonassoc WITH
%nonassoc LARROW RARROW DASH
%left PLUS MINUS
%left TIMES DIVIDE
%right NOT NEG

%start program
%type <Ast.program> program

%%


program:
  decls EOF { $1 }

decls:
   /* nothing */ { [], [] }
 | decls func { ($2 :: fst $1), snd $1 }
 | decls stmt { fst $1, ($2 :: snd $1) }

func:
   func_dec LBRACE  stmt_list RBRACE { func($1, List.rev $3) }

func_dec:
	ID LPAREN formals_opt RPAREN { func_dec($1, $3) }

formals_opt:
    /* nothing */ { [] }
  | formal_list   { List.rev $1 }

formal_list:
    ID                   { [$1] }
  | formal_list COMMA ID { $3 :: $1 }

stmt_list:
    /* nothing */  { [] }
  | stmt_list stmt { $2 :: $1 }

  stmt:
  RETURN expr SEMI { Return $2 }
  | IF LPAREN expr RPAREN LBRACE stmt_list RBRACE { If($3, $6, []) }
  | IF LPAREN expr RPAREN LBRACE stmt_list RBRACE ELSE LBRACE stmt_list RBRACE   { If($3, $6, List.rev $10) }
  | IF LPAREN expr RPAREN LBRACE stmt_list RBRACE ELSE IF LPAREN expr RPAREN LBRACE stmt_list RBRACE  { If($3, List.rev $6, If($11, List.rev $14)) }
  | FOR LPAREN expr SEMI expr SEMI expr RPAREN LBRACE stmt_list RBRACE
     { For($3, $5, $7, List.rev $10) }
  | ID ASSIGN expr SEMI { Assign($1, $3) }
  | WHILE LPAREN expr RPAREN stmt SEMI { While($3, $5) }
  | BREAK SEMI{ Break }
  | CONTINUE SEMI{ Continue }

  expr:
    INTLIT           { IntLit($1) }
  | TRUE             { BoolLit(true) }
  | FALSE            { BoolLit(false) }
  | STRINGLIT        { StrLit($1) }
  | CHARLIT          { CharLit($1) }
  | DOUBLELIT        { FloatLit($1) }      
  | ID               { Id($1) }
  | LBRACKET actuals_opt RBRACKET { List($2)}
  | ID LPAREN actuals_opt RPAREN { Call($1, $3) }
  | ID LBRACKET expr RBRACKET { Item($1, $3) }
  | ID LBRACKET ID COLON expr RBRACKET { Subset($1, $3, $5) }
  | expr DOT ID { Dot($1, $3) }
  | expr PLUS   expr { Binop($1, Add,   $3) }
  | expr MINUS  expr { Binop($1, Sub,   $3) }
  | expr TIMES  expr { Binop($1, Mult,  $3) }
  | expr DIVIDE expr { Binop($1, Div,   $3) }
  | expr EQ     expr { Binop($1, Equal, $3) }
  | expr NEQ    expr { Binop($1, Neq,   $3) }
  | expr LT     expr { Binop($1, Less,  $3) }
  | expr LEQ    expr { Binop($1, Leq,   $3) }
  | expr GT     expr { Binop($1, Greater, $3) }
  | expr GEQ    expr { Binop($1, Geq,   $3) }
  | expr AND    expr { Binop($1, And,   $3) }
  | expr OR     expr { Binop($1, Or,    $3) }
  | expr IN     expr { Binop($1, In,    $3) }
  | MINUS expr %prec NEG { Unop(Neg, $2) }
  | NOT expr         { Unop(Not, $2) }
  | expr STAR        { Unop(Star, $1) }
  | expr LARROW expr with_opt { Edge($1, To, $3, $4) }
  | expr RARROW expr with_opt { Edge($1, From, $3, $4) }
  | expr DASH expr with_opt  { Edge($1, Dash, $3, $4) }
  | LPAREN actuals_opt RPAREN with_opt { Graph($2, $4) }
  | LBRACE rec_opt RBRACE { Record($2) }
  | LPAREN expr RPAREN { $2 }

with_opt:
  /* nothing */ %prec NOWITH { Noexpr }
  | WITH expr { $2 }

actuals_opt:
    /* nothing */ { [] }
  | actuals_list  { List.rev $1 }

actuals_list:
    expr                    { [$1] }
  | actuals_list COMMA expr { $3 :: $1 }

rec_opt:
    /* nothing */ { [] }
  | rec_list  { List.rev $1 }

rec_list:
    expr                    { [$1] }
  | rec_list COMMA expr COLON expr { ($3, $5) :: $1 }
