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

type stmt =
<<<<<<< HEAD
     Asn of id * expr * bool
=======
     Asn of string * expr * bool
|    Expr of expr
>>>>>>> master
|    If of expr * stmt list * stmt list
|    While of expr * stmt list
|    For of expr * expr * expr * stmt list
|    Return of expr
|    Break
|    Continue
  
type stmt_list = stmt list

type func_dec = Fdecl of id * id list
(*name, formals, return type*)
type afunc_dec = AFdecl of id * primitiveType list * primitiveType

type func = Fbody of func_dec * stmt list
type afunc = AFbody of afunc_dec * astmt list

type program = func list


<<<<<<< HEAD

=======
type func = {
    fname : string;
    formals : string list;
    body : stmt list;
  }

type program = func list

(* Pretty-printing function *)

let string_of_op = function
        Add -> "+"
        | Sub -> "-"
        | Mult -> "*"
        | Div -> "/"
        | Equal -> "=="
        | Neq -> "!="
        | Less -> "<"
        | Leq -> "<="
        | Greater -> ">"
        | Geq -> ">="
        | And -> "&&"
        | Or -> "||"
        | Fadd -> ".+"
        | Fsub -> ".-"
        | Fmult -> ".*"
        | Fdiv -> "./"
        (* Gadd, Eadd *)
        
let string_of_uop = function 
        Neg -> "-"
        | Not -> "!"

let string_of_eop = function
        To -> "->"
        | From -> "<-"
        | Dash -> "--"

let rec string_of_expr = function 
        IntLit(i) -> string_of_int i
        | BoolLit(true) -> "true"
        | BoolLit(false) -> "false"
        | StrLit(s) -> s
        | CharLit(c) -> c
        | FloatLit(f) -> string_of_float f
        | Id(s) -> s
      (*| List *)
        | Call(f, el) -> f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
      (*| Item
        | Subset *)
        | Dot -> "."  
        | Binop(e1, o, e2) -> 
                string_of_expr e1 ^ " " ^ string_of_of o ^ " " string_of_expr e2
        | Unop(o, e) -> string_of_uop o ^ " " ^ string_of_expr e2
      (*| Edge
        | Graph
        | Node
        | Record *)
        | Noexpr -> ""


let rec string_of_stmt = function 
        Expr(expr) -> string_of_expr expr ^ ";\n"
        | Return(expr) -> "return " ^ string_of_expr expt ^ ";\n";
    (*  | If(e, s) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
        | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
                string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
        | For(e1, e2, e3, s) ->
                "for (" ^ string_of_expr e1  ^ " ; " ^ string_of_expr e2 ^ " ; " ^
                string_of_expr e3  ^ ") " ^ string_of_stmt s
        | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s
    *)


(* let string_of_typ = function 
        Int -> "int"
        | Bool -> "bool"
        | Void -> "void"
*)


(* let string_of_vdecl (t, id) = string_of_typ t ^ " " ^ id ^ ";\n" *)

         
let string_of_func func = 
  func.fname ^ "(" ^ String.concat ", " (List.map snd func.formals) ^
  ")\n{\n" ^
(*  String.concat "" (List.map string_of_vdecl func.locals) ^ *)
  String.concat "" (List.map string_of_stmt func.body) ^
  "}\n"

let string_of_program (funcs) = 
        String.concat "" (List.map string_of_func funcs) ^ "\n"
>>>>>>> master

