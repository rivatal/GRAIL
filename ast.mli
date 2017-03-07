type eop = To | From | Dash

type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq |
          And | Or

type uop = Neg | Not | Star

type expr =
    IntLit of int
|    BoolLit of bool
|    StrLit of string
|    CharLit of char 
|    FloatLit of float
|    Id of string
|    List of expr list
|    Call of string * expr list
|    Item of string * expr
|    Subset of string * string * expr
|    Dot of expr * string
|    Unop of uop * expr
|    Binop of expr * op * expr
|    Edge of expr * eop * expr * (string * float) list
|    Graph of expr list
|    Record of (string * expr) list
|    Noexpr


type stmt =
    Seq of stmt list
|    Asn of string * expr
|    If of expr * stmt * stmt * stmt
|    While of expr * stmt
|    For of expr * expr * expr * stmt
|    Break
|    Continue
  

type func_dec = string * expr list

type func = func_dec * stmt list

type program = func list * stmt list