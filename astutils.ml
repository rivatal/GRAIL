open Ast

module CharMap = Map.Make(String)
type genericMap = int CharMap.t

(*Let the strings begin *)
let string_of_op (op: op) =
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

let rec string_of_aexpr (ae: aexpr): string =
  match ae with
  | AIntLit(x, t)  -> Printf.sprintf "(%s: %s)" (string_of_int x) (string_of_type t)
  | ABoolLit(b, t) -> Printf.sprintf "(%s: %s)" (string_of_bool b) (string_of_type t)
  | AStrLit(b, t) -> Printf.sprintf "(%s: %s)" (b) (string_of_type t)
  | AId(x, t) -> Printf.sprintf "(%s: %s)" x (string_of_type t)
  | ABinop(e1, op, e2, t) ->
    let s1 = string_of_aexpr e1 in let s2 = string_of_aexpr e2 in
    let sop = string_of_op op in let st = string_of_type t in
    Printf.sprintf "(%s %s %s: %s)" s1 sop s2 st
  | AFun(id, ae, t) ->
    let s1 = string_of_aexpr ae in
    let st = string_of_type t in
    Printf.sprintf "(fun %s -> %s): %s" id s1 st

let rec string_of_expr (e: expr): string =
  match e with
  | IntLit(x) -> string_of_int x
  | BoolLit(b) -> string_of_bool b
  | StrLit(b) -> b
  | Id(s) -> s
  | Binop(e1, op, e2) ->
    let s1 = string_of_expr e1 and s2 = string_of_expr e2 in
    let sop = string_of_op op in
    Printf.sprintf "(%s %s %s)" s1 sop s2
  | Fun(id, e) ->
    let s1 = string_of_expr e in Printf.sprintf "(fun %s -> %s)" id s1 


let rec string_of_stmt = function 
        | AReturn(aexpr,typ) -> "return " ^ string_of_aexpr aexpr ^ " " ^ string_of_type typ ^ ";\n";
        | AAsn(id,aexpr,_,typ) -> id ^ " = " ^ string_of_aexpr aexpr ^ " " ^ string_of_type typ ^ ";\n"
        | AExpr(aexpr,typ) -> " " ^ string_of_aexpr aexpr ^ " " ^ string_of_type typ ^ ";\n";
    (*  | If(e, s) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
        | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
                string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
        | For(e1, e2, e3, s) ->
                "for (" ^ string_of_expr e1  ^ " ; " ^ string_of_expr e2 ^ " ; " ^
                string_of_expr e3  ^ ") " ^ string_of_stmt s
        | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s
    *)

let string_of_func func = 
    "Type :" ^ string_of_type func.typ ^ " Name : " ^ func.fname ^ "(" ^ String.concat ", " (List.map fst func.formals) ^
    ")\n{\n" ^
    (*  String.concat "" (List.map string_of_vdecl func.locals) ^ *)
     String.concat "" (List.map string_of_stmt func.body) ^
    "}\n"

