open Ast

(*Let the strings begin *)
let string_of_op (op: op) =
  match op with
  | Add -> "+" | Mult -> "*" | Less -> "<" | Greater -> ">"
  | Or -> "||" | And -> "&&" | Sub -> "-" | Div -> "/" | Fadd -> ".+"
  | Equal -> "==" | Neq -> "-" | Leq -> "<=" | Geq -> ">=" | Fsub -> ".-"
  | Fmult -> ".*" | Fdiv -> "./" | To -> "<-" | From -> "->" | Dash -> "--"
  | In -> "in" | Gadd -> "&" | Eadd -> ".&"

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
  |AFor(as1, ae1, as2, astmts) ->
    "for (" ^ string_of_astmt as1  ^ string_of_aexpr ae1 ^ " ; " ^ string_of_astmt as2 
    ^ string_of_astmt_list astmts
  |AWhile(ae1, astmts) -> "while (" ^ string_of_aexpr ae1 ^ ") {" ^ string_of_astmt_list astmts ^ "}"

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
    [] -> ""
  |h :: t -> string_of_expr h ^ string_of_expr_list t


let string_of_func (func: sast_afunc) = 
  let header = func.fname in
  let formals = "(" ^ String.concat ", " (List.map fst func.formals) ^ "){ : " ^ string_of_type func.typ ^ "\n"
  in let body = String.concat "" (List.map string_of_astmt func.body) ^ "}\n"
  in header ^ formals ^ body
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
