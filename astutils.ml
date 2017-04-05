open Ast

module CharMap = Map.Make(String)
type genericMap = int CharMap.t

(*Let the strings begin *)
let string_of_op (op: op) =
  match op with
  | Add -> "+" | Mult -> "*" | Less -> "<" | Greater -> ">"
  | Or -> "||" | And -> "&&" | Sub -> "-" | Div -> "/" | Fadd -> ".+"
  | Equal -> "==" | Neq -> "-" | Leq -> "<=" | Geq -> ">=" | Fsub -> ".-"
  | Fmult -> ".*" | Fdiv -> "./" 

let string_of_type (t: primitiveType) =
  let rec aux (t: primitiveType) (chr: int) (map: genericMap) =
    match t with
    | TInt -> "int", chr, map
    | TBool -> "bool", chr, map
    | TFloat -> "float", chr, map
    | TString -> "str", chr, map
    | TChar -> "char", chr, map
    | T(x) ->
      let gen_chr, new_chr, new_map = if CharMap.mem x map
        then Char.escaped (Char.chr (CharMap.find x map)), chr, map
        else
          let c = Char.escaped (Char.chr chr) in
          c, (chr + 1), CharMap.add x chr map
      in
      Printf.sprintf "'%s" gen_chr, new_chr, new_map
  in let s, _, _ = aux t 97 CharMap.empty in s

(*^^What does this even do??*)
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
  | ABinop(e1, op, e2, t) ->
    let s1 = string_of_aexpr e1 in let s2 = string_of_aexpr e2 in
    let sop = string_of_op op in let st = string_of_type t in
    Printf.sprintf "(%s %s %s: %s)" s1 sop s2 st
  | ACall(id, astmts, t) ->
    let allaexprs = 
      let rec matchlist m= 
        match m with
          [] -> []
        | hd :: tl ->
          string_of_stmt hd ::  matchlist tl
      in String.concat "" (matchlist astmts)  
    in Printf.sprintf "%s (%s): %s" id allaexprs (string_of_type t)

and string_of_stmt l= 
  match l with 
  | AReturn(aexpr,typ) -> "return " ^ string_of_aexpr aexpr ^ "; " ^ string_of_type typ ^ "\n";
  | AAsn(id,aexpr,_) -> id ^ " = " ^ string_of_aexpr aexpr ^ "; ";
  | AExpr(aexpr) -> " " ^ string_of_aexpr aexpr ^ "; "
(*  | If(e, s) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
    | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
            string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
    | For(e1, e2, e3, s) ->
            "for (" ^ string_of_expr e1  ^ " ; " ^ string_of_expr e2 ^ " ; " ^
            string_of_expr e3  ^ ") " ^ string_of_stmt s
    | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s
*)

and string_of_ustmt (l: stmt)= 
  match l with 
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n";
  | Asn(id,expr,_) -> id ^ " = " ^ string_of_expr expr ^ ";\n"
  | Expr(expr) -> " " ^ string_of_expr expr ^ ";\n"
  
and string_of_expr (e: expr): string =
  match e with
  | IntLit(x) -> string_of_int x
  | BoolLit(b) -> string_of_bool b
  | StrLit(b) -> b
  | FloatLit(f) -> string_of_float f
  | CharLit(c) -> String.make 1 c
  | Id(s) -> s
  | Binop(e1, op, e2) ->
    let s1 = string_of_expr e1 and s2 = string_of_expr e2 in
    let sop = string_of_op op in
    Printf.sprintf "(%s %s %s)" s1 sop s2
  | Call(id, e) ->
    let s1 = List.map(fun a -> (string_of_expr (a))) e in let l = String.concat "," s1 in Printf.sprintf "(call %s(%s))" id l 

let string_of_func (func: sast_afunc) = 
  let header = func.fname in
  let formals = "(" ^ String.concat ", " (List.map fst func.formals) ^ "){ : " ^ string_of_type func.typ ^ "\n"
  in let body = String.concat "" (List.map string_of_stmt func.body) ^ "}\n"
in header ^ formals ^ body
(*   let t = "Type :" ^ string_of_type func.typ 
  in let name = 
       " Name : " ^ func.fname
  in let formals = "(" ^ String.concat ", " (List.map fst func.formals) ^ ")\n{\n"
  in let body = 
       String.concat "" (List.map string_of_stmt func.body) ^ "}\n"
  in t ^ name ^ formals ^body
 *)
