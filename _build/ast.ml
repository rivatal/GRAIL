type id = string

type eop = To | From | Dash

type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq |
          And | Or | In | Fadd | Fsub | Fmult | Fdiv | Gadd | Eadd

type uop = Neg | Not 


type primitiveType =
    | TVoid
    | TInt
    | TBool
    | TString
    | T of string
    | TFun of primitiveType * primitiveType
    | TStmt of primitiveType * primitiveType

    (* annotated expr -> expr with types *)
type aexpr =
    | AIntLit of int * primitiveType
    | ABoolLit of bool * primitiveType
    | AStrLit of string * primitiveType
    | AId of string * primitiveType
    | ABinop of aexpr * op * aexpr * primitiveType
    | AFun of id * aexpr * primitiveType
    | ACall of string * aexpr list * primitiveType


type expr =
     IntLit of int
|    BoolLit of bool
|    StrLit of string
|    CharLit of char 
|    FloatLit of float
|    Id of string
|    List of expr list
|    Fun of id * expr (* Lambda Function *)
|    Call of string * expr list
|    Item of string * expr
|    Subset of string * string * expr
|    Dot of expr * string
|    Unop of uop * expr
|    Binop of expr * op * expr
|    Edge of expr * eop * expr * expr
|    Graph of expr list * expr
|    Node of string * expr
|    Record of (string * expr) list
|    Noexpr

type astmt =
| AAsn of id * aexpr * bool * primitiveType
| AReturn of aexpr * primitiveType
| AExpr of aexpr * primitiveType

type stmt =
| Asn of id * expr * bool
| If of expr * stmt list * stmt list
| While of expr * stmt list
| For of expr * expr * expr * stmt list
| Return of expr
| Break
| Continue
| Expr of expr

  
type stmt_list = stmt list

type func_dec = Fdecl of id * id list

(*name, formals, return type*)
type afunc_dec = AFdecl of id * id list * primitiveType list * primitiveType

type func = Fbody of func_dec * stmt list
type afunc = AFbody of afunc_dec * astmt list

type sast_afunc = { 
    typ : primitiveType; 
    fname : string;
    formals : (string * primitiveType) list;
    body: astmt list
}

type program = func list
