open Ast
open Astutils

module NameMap = Map.Make(String)
module GlobalMap = Map.Make(String)
type genvironment = (primitiveType * primitiveType list * stmt list) GlobalMap.t

let parse (s: string) : Ast.program =
  Parser.program Scanner.token (Lexing.from_string s)

(*Prints the name of an unannotated function*)
let print_func (func: Ast.func): unit =
  match func with
    Fbody(Fdecl(name, formals), astmts) -> 
    print_endline ("Function " ^ name);
    List.iter (fun a -> (print_endline a)) formals;
    List.iter (fun a -> (print_endline (string_of_ustmt a))) astmts


(*Prints an annotated function*)
let print_afunc (afunc: Ast.afunc): unit =
  match afunc with
    AFbody(AFdecl(name, _, formals, t), astmts) -> 
    print_endline ("Function " ^ name ^ " " ^ string_of_type t);
    List.iter (fun a -> (print_endline (string_of_type (a)))) formals;
    List.iter (fun a -> (print_endline (string_of_type (Infer.type_of_stmt a)))) astmts

let rec print_afunc_list (ast: Ast.afunc list): unit =
  match ast with
    [] -> ()
  |hd :: tl ->
    print_afunc hd; 
    print_afunc_list tl


let rec get_ids_expr (e: expr): string list =
  match e with
  | IntLit(_) | BoolLit(_) | StrLit(_) -> []
  | Id(x) -> [x]
  | Fun(x, y) -> [x] @ (get_ids_expr y)
  | Binop(e1, _, e2) -> (get_ids_expr e1) @ (get_ids_expr e2)
  | Call(id, elist) -> id :: (get_ids_list elist)
and get_ids_list (e: expr list): string list =
  match e with 
    [] -> []
  |h :: t -> get_ids_expr h @ get_ids_list t


let rec get_ids_formals(e: string list) =
  match e with 
    [] -> []
  |h :: t -> h :: get_ids_formals t

let rec get_all_ids (e: stmt list): string list =
  match e with 
  | [] -> []
  | hd :: tl ->
    match hd with
    | Asn(x, y, _) -> [x] @ (get_ids_expr y) @ get_all_ids tl 
    | Return(x) -> (get_ids_expr x) @ get_all_ids tl
    | Expr(x) -> (get_ids_expr x) @ get_all_ids tl


let get_all_formals_ids (e: func): (string list * string) =
  match e with 
  |Fbody(Fdecl(name, formals), stmts) ->
    let rec dedup = function
      | [] -> []
      | x :: y :: xs when x = y -> y :: dedup xs
      | x :: xs -> x :: dedup xs
    in 
    let ids1 = get_all_ids stmts 
    in let ids2 = get_ids_formals formals
    in (dedup (ids1 @ ids2),name)

let infer (e: Ast.func) (genv : genvironment) : (Ast.afunc * genvironment) =
  let vals, fname = get_all_formals_ids e in
  let env = List.fold_left (fun m x -> NameMap.add x (Infer.gen_new_type ()) m) NameMap.empty vals in 
  let genv = GlobalMap.add fname (Infer.gen_new_type (),[],[]) genv in 
  Infer.infer_func e env genv

let infer_func (e: Ast.func) (genv :  genvironment): (genvironment * Ast.afunc) = 
  let (afunc,genv) = infer e genv in (genv, afunc)


let grail (ast: Ast.afunc list) (input: string) : Ast.afunc list =
  let rec do_program(p: Ast.program) (genv : genvironment) : Ast.afunc list  =   
    match p with
      [] -> []
    |hd :: tl -> let (genv, afunc) =
                   infer_func hd genv 
      in afunc :: do_program tl genv
  in 
  let genv = GlobalMap.add "print" (TInt, [TString], []) GlobalMap.empty in 
  do_program (parse input) genv

let format_sast_codegen (ast : Ast.afunc) : Ast.sast_afunc = 
  match ast with 
    AFbody(AFdecl(name, formals, aformals, t), astmts) ->
    { typ = t; 
      fname = name;
      formals = List.combine formals aformals;
      body = astmts
    }

let compile() = grail [] "main() { print(\"Hello, World!\n\"); }";;
compile();


(* let compile() = let sast = List.map format_sast_codegen (grail [] "main() { print(\"Hello, World!\n\"); }") in
  let m = Codegen.translate sast in
  Llvm_analysis.assert_valid_module m; print_string (Llvm.string_of_llmodule m) ;;
compile();
 *)