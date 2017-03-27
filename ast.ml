module CharMap = Map.Make(String)
type genericMap = int CharMap.t

type id = string
type eop = To | From | Dash

type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq |
          And | Or | In | Fadd | Fsub | Fmult | Fdiv | Gadd | Eadd

type uop = Neg | Not 


type primitiveType =
    | TInt
    | TBool
    | TString
    | T of string
    | TFun of primitiveType * primitiveType

    (* annotated expr -> expr with types *)
type aexpr =
    | AIntLit of int * primitiveType
    | ABoolLit of bool * primitiveType
    | AStrLit of string * primitiveType
    | AId of string * primitiveType
    | ABinop of aexpr * op * aexpr * primitiveType
    | ACall of string * aexpr list
    | AFun of id * aexpr * primitiveType

type whatevs = 
    aexpr list

(*     | ACall of id * (aexpr list) * primitiveType  *)(*Function call-- id = print, print's t is void, we need it to have void

      What to do when we see call:
      main(3) (*go look at main the function and check that the formal is actually an int. 
      Assign call's primitive type to be the function type. *)

    *)


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
| AAsn of string * aexpr * bool * primitiveType


type stmt =
     Asn of string * expr * bool
|    If of expr * stmt list * stmt list
|    While of expr * stmt list
|    For of expr * expr * expr * stmt list
|    Return of expr
|    Break
|    Continue
  

type func_dec = string * string list

type func = func_dec * stmt list

type program = func list
