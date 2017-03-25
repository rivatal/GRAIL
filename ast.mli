type eop = To | From | Dash

type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq |
          And | Or | In | Fadd | Fsub | Fmult | Fdiv | Gadd | Eadd

type uop = Neg | Not 

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
|    Edge of expr * eop * expr * expr
|    Graph of expr list * expr
|    Node of string * expr
|    Record of (string * expr) list
|    Noexpr



type stmt =
     Asn of string * expr * bool
|    Expr of expr
|    If of expr * stmt list * stmt list
|    While of expr * stmt list
|    For of expr * expr * expr * stmt list
|    Return of expr
|    Break
|    Continue
  

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
        IntLit(i) -> i
        | BoolLit(true) -> "true"
        | BoolLit(false) -> "false"
        | StrLit(s) -> s
        | CharLit(c) -> c
     (* | FloatLit(f) -> f *)
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

         
let string_of_fdecl fdecl = 
  string_of_typ fdecl.typ ^ " " ^
  fdecl.fname ^ "(" ^ String.concat ", " (List.map snd fdecl.formals) ^
  ")\n{\n" ^
(*  String.concat "" (List.map string_of_vdecl fdecl.locals) ^ *)
  String.concat "" (List.map string_of_stmt fdecl.body) ^
  "}\n"

let string_of_program (funcs) = 
        String.concat "" (List.map string_of_fdecl funcs) ^ "\n"










