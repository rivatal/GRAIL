type id = string

type eop = To | From | Dash

type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq |
          And | Or | In | Fadd | Fsub | Fmult | Fdiv | Gadd | Eadd

type uop = Neg | Not 

type primitiveType =
  | TInt
  | TBool
  | TString
  | TFloat
  | TChar 
  | T of string
  | TVoid
  | TList of primitiveType
  | TAssoc of primitiveType
  | TRec of primitiveType

type expr =
    IntLit of int
  | BoolLit of bool
  | StrLit of string
  | CharLit of char 
  | FloatLit of float
  | Id of string
  | List of expr list
  | Call of string * expr list
  | Item of string * expr
  | Subset of string * string * expr
  | Dot of expr * string
  | Unop of uop * expr
  | Binop of expr * op * expr
  | Edge of expr * eop * expr * expr
  | Graph of expr list * expr
  | Node of string * expr
  | Record of (string * expr) list
  | Noexpr

(* annotated expr -> expr with types *)
type aexpr =
  | AIntLit of int * primitiveType
  | ACharLit of char * primitiveType
  | ABoolLit of bool * primitiveType
  | AStrLit of string * primitiveType
  | AFloatLit of float * primitiveType
  | AId of string * primitiveType
  | ABinop of aexpr * op * aexpr * primitiveType
  | AUnop of uop * aexpr * primitiveType
  | ACall of string * astmt list * primitiveType  
  | AList of aexpr list * primitiveType         (*Make sure to check that the primitive type is only a TList*)
  | AItem of string * aexpr * primitiveType
  | ARecord of astmt list * primitiveType      
  | ADot of aexpr * string * primitiveType

and astmt =
  | AAsn of id * aexpr * bool * primitiveType
  | AIf of aexpr * astmt list * astmt list
  | AFor of astmt * aexpr * astmt * astmt list
  | AReturn of aexpr * primitiveType
  | ABreak
  | AContinue
  | AExpr of aexpr

and stmt =
  | Asn of id * expr * bool
  | If of expr * stmt list * stmt list
  | While of expr * stmt list
  | For of stmt * expr * stmt * stmt list
  | Return of expr
  | Break
  | Continue
  | Expr of expr

type stmt_list = stmt list

type func_dec = Fdecl of id * id list

(*name, formals, return type*)
type afunc_dec = AFdecl of id * (id * primitiveType) list * primitiveType

type func = Fbody of func_dec * stmt list
type afunc = AFbody of afunc_dec * astmt list



type sast_afunc = { 
  typ : primitiveType; 
  fname : string;
  formals : (string * primitiveType) list;
  body: astmt list
}

type program = func list

