(* Need to fix syntax and funcrion to match GRAIL syntax *)

open Ast

(*Let the strings begin *)
let string_of_op (op: op) =
  match op with
  | Add -> "+" | Mult -> "*" | Less -> "<" | Greater -> ">"
  | Or -> "||" | And -> "&&" | Sub -> "-" | Div -> "/" | Fadd -> ".+"
  | Equal -> "==" | Neq -> "-" | Leq -> "<=" | Geq -> ">=" | Fsub -> ".-"
  | Fmult -> ".*" | Fdiv -> "./" | To -> "<-" | From -> "->" | Dash -> "--"
  | In -> "in" | Gadd -> "&" | Eadd -> ".&" | Ladd -> "^"

let string_of_uop (uop: uop) =
  match uop with
  |Neg -> "-"
  |Not -> "not "

let rec string_of_type (t: primitiveType) =
  match t with
  | TRec(s, l) -> (Printf.sprintf "record %s" s)
  | TInt -> "int"
  | TBool -> "bool"
  | TFloat -> "float"
  | TString -> "str"
  | TChar -> "char"
  | TVoid -> "void"
  | TEdge(x) -> "edge of " ^ (string_of_type x)
  | TGraph(a, b) -> "graph of " ^ (string_of_type a) ^ " with " ^ (string_of_type b)
  | TList(x) -> "list of " ^ (string_of_type x)
  | T(x) -> Printf.sprintf "%s" x

let string_of_tuple (t: id * primitiveType) =
  match t with
    (a, b) -> a ^ " " ^ string_of_type b

let rec string_of_aexpr (ae: aexpr): string =
  match ae with
  | AIntLit(x, t)  -> Printf.sprintf "(%s: %s)" (string_of_int x) (string_of_type t)
  | ABoolLit(b, t) -> Printf.sprintf "(%s: %s)" (string_of_bool b) (string_of_type t)
  | AFloatLit(f, t) -> Printf.sprintf "(%s: %s)" (string_of_float f) (string_of_type t)
  | AStrLit(b, t) -> Printf.sprintf "(%s: %s)" (b) (string_of_type t)
  | ACharLit(c, t) -> Printf.sprintf "(%s: %s)" (String.make 1 c) (string_of_type t)
  | AId(x, t) -> Printf.sprintf "(%s: %s)" x (string_of_type t)
  | ADot(s,entry,t) -> Printf.sprintf "(%s.%s : %s)" (string_of_aexpr s) entry (string_of_type t)
  | AItem(s, e1, t) -> Printf.sprintf "(%s[%s] : %s)" s (string_of_aexpr e1) (string_of_type t)
  (*   | ASubset(_,_,t) -> Printf.sprintf "(%s)" (string_of_type t)
  *)  | ABinop(e1, op, e2, t) ->
    let s1 = string_of_aexpr e1 in let s2 = string_of_aexpr e2 in
    let sop = string_of_op op in let st = string_of_type t in
    Printf.sprintf "(%s %s %s: %s)" s1 sop s2 st
  | AUnop(op, e1, t) ->
    let s1 = string_of_aexpr e1 in let sop = string_of_uop op in let st = string_of_type t in
    Printf.sprintf "(%s%s: %s)" sop s1 st
  | ACall(id, _, astmts, t) ->
    let allaexprs = 
      let rec matchlist m= 
        match m with
          [] -> []
        | hd :: tl ->
          string_of_astmt hd ::  matchlist tl
      in String.concat "" (matchlist astmts)  
    in Printf.sprintf "%s (%s): %s" id allaexprs (string_of_type t)
  | ARecord(aexprs, t) ->
    let rec helper l str : string =
      (match l with
         [] -> str
       |(id, aexpr) :: t -> helper t (id ^ " " ^ string_of_aexpr aexpr ^ str))
    in 
(*     ignore(print_string ("list is length " ^ string_of_int (List.length aexprs))); *)
    ((string_of_type t) ^ "{" ^ (helper aexprs "") ^ "}")
  | AEdge(e1, op, e2, e3, t) -> Printf.sprintf "%s %s %s %s : %s" (string_of_aexpr e1) (string_of_op op) (string_of_aexpr e2) (string_of_aexpr e3) (string_of_type t)
  | AList(elist, t) -> Printf.sprintf "(%s : %s)" (string_of_aexpr_list elist) (string_of_type t)
  | AGraph(elist, e1, t) -> Printf.sprintf "(%s %s : %s)" (string_of_aexpr_list elist) (string_of_aexpr e1) (string_of_type t)
  | ANoexpr(_) -> ""

and string_of_aexpr_list l =
  match l with
    [] -> ""
  |h :: t -> string_of_aexpr h ^ string_of_aexpr_list t

and string_of_astmt (l: astmt) = 
  match l with 
  | AReturn(aexpr,typ) -> "return " ^ string_of_aexpr aexpr ^ "; " ^ string_of_type typ ^ "\n";
  | AAsn(ae1,ae2,_,_) -> string_of_aexpr ae1 ^ " = " ^ string_of_aexpr ae2 ^ "; ";
  | AExpr(aexpr) -> " " ^ string_of_aexpr aexpr ^ "; "
  | AIf(e, s1, s2) ->  
    let a = "if (" ^ string_of_aexpr e ^ ") {" ^ string_of_astmt_list s1 ^ "; " in
    let b =  (match s2 with
          [] -> ""
        |rest -> string_of_astmt_list rest) in (a ^ b)
  | AFor(as1, ae1, as2, astmts) ->
    "for (" ^ string_of_astmt as1  ^ string_of_aexpr ae1 ^ " ; " ^ string_of_astmt as2 
    ^ string_of_astmt_list astmts
  | AWhile(ae1, astmts) -> "while (" ^ string_of_aexpr ae1 ^ ") {" ^ string_of_astmt_list astmts ^ "}"
  | AForin(id, aexpr, astmts) -> "for (" ^ string_of_aexpr id ^ " in " ^ string_of_aexpr aexpr ^ "){" ^ string_of_astmt_list astmts

and string_of_astmt_list (stmts : astmt list) : string =
  let s1 = List.map(fun a -> (string_of_astmt (a))) stmts in let l = String.concat "" s1 in l

and string_of_stmt (l: stmt)= 
  match l with 
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n";
  | Asn(e1,e2,_) -> string_of_expr e1 ^ " = " ^ string_of_expr e2 ^ ";\n"
  | Expr(expr) -> " " ^ string_of_expr expr ^ ";\n"
  | If(e, s1,  s2) -> let a = "if (" ^ string_of_expr e ^ ") {" ^ string_of_stmt_list s1 ^ "; }" in
    let b =
      match s2 with
        [] -> ""
      |rest -> string_of_stmt_list rest in 
    (a ^ b)
  | For(s1, e1, s2, astmts) -> "for (" ^ string_of_stmt s1 ^ string_of_expr e1 ^ string_of_stmt s2 ^" ) {\n" ^
                               string_of_stmt_list astmts ^ "}" 
  | While(e1, stmts) -> "while (" ^ string_of_expr e1 ^ "){\n" ^ string_of_stmt_list stmts ^ "}"
  | Forin(s, e, stmts) -> "for (" ^ string_of_expr s ^ " in " ^ string_of_expr e ^ "){" ^ string_of_stmt_list stmts

and string_of_stmt_list (stmts : stmt list) : string =
  let s1 = List.map(fun a -> (string_of_stmt (a))) stmts in let l = String.concat "" s1 in l

and string_of_expr (e: expr): string =
  match e with
  | IntLit(x) -> string_of_int x
  | BoolLit(b) -> string_of_bool b
  | StrLit(b) -> b
  | FloatLit(f) -> string_of_float f
  | CharLit(c) -> String.make 1 c
  | Id(s) -> s
  | Dot(a, b) -> ((string_of_expr a) ^ "." ^ b)
  (*   | Subset(s,e) -> Printf.sprintf "%s[%s]" s (string_of_expr e)*)  
  | Binop(e1, op, e2) ->
    let s1 = string_of_expr e1 and s2 = string_of_expr e2 in
    let sop = string_of_op op in
    Printf.sprintf "(%s %s %s)" s1 sop s2
  | Unop(uop, e1) ->
    let s1 = string_of_expr e1 in 
    let sop = string_of_uop uop in 
    (Printf.sprintf "(%s%s)" sop s1)
  | Call(id, e) ->
    let s1 = List.map(fun a -> (string_of_expr (a))) e in let l = String.concat "," s1 in Printf.sprintf "(call %s(%s))" id l 
  | Record(exprs) ->
    let rec helper l str : string =
      (match l with
         [] -> str
       |(s, e) :: t -> helper t (str ^ s ^ ": " ^ (string_of_expr e)))
    in ("{" ^ (helper exprs "") ^ "}")
  | Edge(e1, op, e2, e3) -> Printf.sprintf "%s %s %s %s" (string_of_expr e1) (string_of_op op) (string_of_expr e2) (string_of_expr e3)
  | List(elist) -> Printf.sprintf "(%s)" (string_of_expr_list elist)
  | Item(l, e) -> Printf.sprintf "%s[%s]" l (string_of_expr e)
  | Graph(elist, e) -> Printf.sprintf "(%s) with %s" (string_of_expr_list elist) (string_of_expr e)
  | Noexpr -> ""

and string_of_expr_list l =
  match l with
   | [] -> ""
   | h :: t -> string_of_expr h ^ string_of_expr_list t

and string_of_program (p: func list) = 
  let p1 = List.map(fun a -> (string_of_func (a))) p in let l = String.concat "" p1 in l

(*  and string_of_stmt_list (stmts : stmt list) : string =
  let s1 = List.map(fun a -> (string_of_stmt (a))) stmts in let l = String.concat "" s1 in l

and string_of_func_list (funclist: func list) = 
    match funclist with 
      | [] -> ""
      | h :: t -> string_of_func h ^ string_of_func_list t *)

and string_of_func (f: func) =
  match f with  
  | Fbody(a, b) -> string_of_func_dec a ^ " " ^ string_of_stmt_list b
  | _ -> ""

and string_of_func_dec (d: func_dec) = 
  match d with
  | Fdecl(a, blist) -> Printf.sprintf "%s (%s)" (a) (string_of_id_list blist)
  | _ -> ""

and string_of_id_list (dlist: id list) =
  match dlist with
    | [] -> ""
    | h :: t -> Printf.sprintf "%s, %s" (h) (string_of_id_list t)

(* 
type program = func list
type func = Fbody of func_dec * stmt list
type func_dec = Fdecl of id * id list
type stmt_list = stmt list *)


 (* 
  let header = func.fname in
  let formals = "(" ^ String.concat ", " (List.map fst func.formals) ^ "){ : " ^ string_of_type func.typ ^ "\n"
  in let body = String.concat "" (List.map string_of_astmt func.body) ^ "}\n"
  in header ^ formals ^ body *)
(*   let t = "Type :" ^ string_of_type func.typ 
     in let name = 
       " Name : " ^ func.fname
     in let formals = "(" ^ String.concat ", " (List.map fst func.formals) ^ ")\n{\n"
     in let body = 
       String.concat "" (List.map string_of_astmt func.body) ^ "}\n"
     in t ^ name ^ formals ^body
*)
(*Maps a variable to its name in the environment*)
let map_id_with (fname: string )(id: string) : string =
(*    ignore(print_string("map_id_with " ^ fname ^ "#" ^ id ^ "\n"));  *)
  (fname ^ "#" ^ id)


(*
open Ast
open Printf

(* Unary operators *)
let txt_of_unop = function
  | Not -> "Not"
  | Neg -> "Sub"

(* Binary operators *)
let txt_of_binop = function
  (* Arithmetic *)
  | Add -> "Add"
  | Sub -> "Sub"
  | Mult -> "Mult"
  | Div -> "Div"
  (* Boolean *)
  | Or -> "Or"
  | And -> "And"
  | Equal -> "Equal"
  | Neq -> "Neq"
  | Less -> "Less"
  | Leq -> "Leq"
  | Greater -> "Greater"
  | Geq -> "Geq"
  (* Graph *)

(*
let txt_of_graph_op = function
  | Right_Link -> "RLink"
  | Left_Link -> "LLink"
  | Double_Link -> "DLink" *)

let txt_of_var_type = function
  | IntLit(_) -> "int"
  | FloatLit(_) -> "float"
  | StrLit(_) -> "string"
  | BoolLit(_) -> "bool"
 (* | Node_t -> "node"
  | Graph_t -> "graph"
  | Dict_Int_t -> "dict<int>"
  | Dict_Float_t -> "dict<float>"
  | Dict_String_t -> "dict<string>"
  | Dict_Node_t -> "dict<node>"
  | Dict_Graph_t -> "dict<graph>"
  | List_Int_t -> "list<int>"
  | List_Float_t -> "list<float>"
  | List_Bool_t -> "list<bool>"
  | List_String_t -> "list<string>"
  | List_Node_t -> "list<node>"
  | List_Graph_t -> "list<graph>"  *)

(*
let txt_of_formal = function
| Formal(vtype, name) -> sprintf "Formal(%s, %s)" (txt_of_var_type vtype) name
*)

let txt_of_formal_list formals =
  let rec aux acc = function
    | [] -> sprintf "%s" (String.concat ", " (List.rev acc))
  (*  | fml :: tl -> aux (txt_of_formal fml :: acc) tl *)
  in aux [] formals

let txt_of_num x = function
  IntLit(x) -> string_of_int x
(*  | FloatLit(x) -> string_of_float x *)

(* Expressions *)
let rec txt_of_expr = function
  | IntLit(x) -> sprintf "Int_Lit(%s)" (string_of_int x)
  | BoolLit(x) -> sprintf "Bool_lit(%s)" (string_of_bool x) 
  | StrLit(x) -> sprintf "String_Lit(%s)" x 
(*  | EdgeAt(g, a, b) -> sprintf "%s@(%s, %s)" (txt_of_expr g) (txt_of_expr a) (txt_of_expr b) 
  | Node(x) -> sprintf "Node(%s)" (txt_of_expr x) *)
  | Unop(op, e) -> sprintf "Unop(%s, %s)" (txt_of_unop op) (txt_of_expr e) 
  | Binop(e1, op, e2) -> sprintf "Binop(%s, %s, %s)"
      (txt_of_expr e1) (txt_of_binop op) (txt_of_expr e2) 
(*  | Graph_Link(e1, op1, e2, e3) -> sprintf "Graph_Link(%s, %s, %s, WithEdge, %s)"
      (txt_of_expr e1) (txt_of_graph_op op1) (txt_of_expr e2) (txt_of_expr e3)  *)
  | Id(x) -> sprintf "Id(%s)" x 
  | Noexpr -> sprintf "Noexpression" 
 (* | ListP(l) -> sprintf "List(%s)" (txt_of_list l)
  | DictP(d) -> sprintf "Dict(%s)" (txt_of_dict d) *)
  | Call(f, args) -> sprintf "Call(%s, [%s]）" (f) (txt_of_list args)
 (* | CallDefault(e, f, args) -> sprintf "CallDefault(%s, %s, [%s])" (txt_of_expr e) f (txt_of_list args) *)

(*Variable Declaration 
and txt_of_var_decl = function
  | Local(var, name, e1) -> sprintf "Local(%s, %s, %s)"
    (txt_of_var_type var) name (txt_of_expr e1) *)

(* Lists *)
and txt_of_list = function
  | [] -> ""
  | [x] -> txt_of_expr x
  | _ as l -> String.concat ", " (List.map txt_of_expr l) 

(* Dict 
and txt_of_dict_key_value = function
  | (key, value) -> sprintf "key:%s,value:%s" (txt_of_expr key) (txt_of_expr value)

and txt_of_dict = function
  | [] -> ""
  | [x] -> txt_of_dict_key_value x
  | _ as d -> String.concat ", " (List.map txt_of_dict_key_value d) *)


(* Statements *)
and txt_of_stmt = function
  | Expr(expr) -> sprintf "Expr(%s);" (txt_of_expr expr)
  | Asn(e1,e2,t) -> sprintf "Asn(%s = %s)" (txt_of_expr e1) (txt_of_expr e2) 
  | Return(expr) -> sprintf "Return;" (txt_of_expr expr) 
  | For(e1,e2,e3,s) -> sprintf "For(%s;%s;%s){%s}"
    (txt_of_expr e1) (txt_of_expr e2) (txt_of_expr e3) (txt_of_stmts s)
  | Forin(e1,e2,s) -> sprintf "For(%s;%s){%s}"
    (txt_of_expr e1) (txt_of_expr e2) (txt_of_stmts s)
  | If(e1,s1,s2) -> sprintf "If(%s){%s} Else{%s}"
    (txt_of_expr e1) (txt_of_stmts s1) (txt_of_stmts s2)
  | While(e1, s) -> sprintf "While(%s){%s}"
    (txt_of_expr e1) (txt_of_stmts s)
and txt_of_stmts stmts =
  let rec aux acc = function
      | [] -> sprintf "%s" (String.concat "\n" (List.rev acc))
      | stmt :: tl -> aux (txt_of_stmt stmt :: acc) tl
  in aux [] stmts



(* Functions Declaration *)
and txt_of_func_decl  = function
  | Fdecl(name, args) -> sprintf "function decl s% (%s)" name (txt_of_expr args)

(* Function Full Definition *)
and txt_of_func = function
  | Fbody(dec, s) -> sprintf "Func_body(%s){%s}" (txt_of_func_decl dec) (txt_of_stmts s)



let funclist_to_afunclist (ast: Ast.func list) = 
  match ast with
  | patt -> expr
  | _ -> expr2


let afunclist_to_afuncdecl (ast: Ast. afunc list) = 


let afuncdecl_to_sast_afunc (ast: Ast.afunc decl) =  *)


(* func list -> afunc list ->  afunc decl -> translate to sast_afunc  -> call string_of_func program

 let format_sast_codegen (ast : Ast.afunc) : Ast.sast_afunc = 
  match ast with 
    AFbody(AFdecl(name, aformals, t), astmts) ->
    { typ = t; 
      fname = name;
      formals = aformals;
      body = astmts
    }

type sast_afunc = { 
  typ : primitiveType; 
  fname : string;
  formals : (string * primitiveType) list;
  body: astmt list
}
 *)

(* Program entry point *)
let _ =
  let lexbuf = Lexing.from_channel stdin in
  let program = Parser.program Scanner.token lexbuf in
  let result = string_of_program program in 
  print_endline result





