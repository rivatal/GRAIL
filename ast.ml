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
    | AFun of id * aexpr * primitiveType

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


(*Let the strings begin *)
(* let string_of_op (op: op) =
  match op with
  | Add -> "+" | Mult -> "*" | Less -> "<" | Greater -> ">"
  | Or -> "||" | And -> "&&"

let string_of_type (t: primitiveType) =
  let rec aux (t: primitiveType) (chr: int) (map: genericMap) =
    match t with
    | TInt -> "int", chr, map
    | TBool -> "bool", chr, map
    | TString -> "str", chr, map
    | T(x) ->
      let gen_chr, new_chr, new_map = if CharMap.mem x map
        then Char.escaped (Char.chr (CharMap.find x map)), chr, map
        else
          let c = Char.escaped (Char.chr chr) in
          c, (chr + 1), CharMap.add x chr map
      in
      Printf.sprintf "'%s" gen_chr, new_chr, new_map
    | TFun(t1, t2) -> let (st1, c1, m1) = aux t1 chr map in
      let (st2, c2, m2) = aux t2 c1 m1 in
      (Printf.sprintf "(%s -> %s)" st1 st2), c2, m2 in
  let s, _, _ = aux t 97 CharMap.empty in s

 *)