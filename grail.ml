open Ast
open Astutils

module NameMap = Map.Make(String)

type environment = primitiveType NameMap.t

module GlobalMap = Map.Make(String)

type genvironment = (primitiveType * (string * primitiveType) list * stmt list) GlobalMap.t

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
    AFbody(AFdecl(name, aformals, t), astmts) -> 
    print_endline ("Function " ^ name ^ " " ^ string_of_type t);
    List.iter (fun a -> (print_endline (string_of_tuple (a)))) aformals;
    List.iter (fun a -> (print_endline (string_of_type (Infer.type_of_stmt a)))) astmts

let rec print_afunc_list (ast: Ast.afunc list): unit =
  match ast with
    [] -> ()
  |hd :: tl ->
    print_afunc hd; 
    print_afunc_list tl


let gen_new_name () =   (*Need to figure out where to reset this*)
  let c1 = !name_variable in
  incr name_variable; Char.escaped (Char.chr c1)

let rec replace_ids_stmts (s: stmt list) (env: environment) (map: environment) = 
  [] -> []
  |h :: t ->
  let (rstmt, map, env) = replace_ids_stmt h env map in [(rstmt, env)] @ replace_ids_stmts t env map 

let rec replace_ids_stmt (s: stmt)(env: environment) (map: environment) = 
  match s with
  Asn(id, expr, switch) -> 
    let (rexpr, map, env) = replace_ids_expr expr env map in
    if NameMap.mem id map
    then (let x = NameMap.find id map)
    else (let x = gen_new_name() in let map = NameMap.add id x map in let env = NameMap.add x gen_new_type()) 
  in (Asn(x, rexpr, switch), map, env)
  |Return(expr) -> let (rexpr, map, env) = replace_ids_expr expr env map in (Return(rexpr), map, env)
  |Expr(expr) -> let (rexpr, map, env) = replace_ids_expr expr env map in (Expr(rexpr), map, env)

(*I think we need to pass the namemap down and up too.*)
let rec replace_ids_expr(e: expr) (env: environment) (map: environment)= 
  match e with
  | Id(x) -> if NameMap.mem x map
    then (Id(NameMap.find x map), map, env)
    else (let x = gen_new_name() in let map = NameMap.add x map in let env = NameMap.add x gen_new_type()) 
    in (Id(x), map, env))
  | Binop(e1, op, e2) ->
    let et1 = replace_ids_expr e1 env map
    and et2 = replace_ids_expr e1 env map
    in (Binop(et1, op, et2), map, env)
  | x -> x  (*Literal or call, just return itself-- but wait, what if it's a call in a function???*)

let rec get_ids_expr (e: expr) (genv: genvironment): string list =
  match e with
  | IntLit(_) | BoolLit(_) | StrLit(_) -> []
  | Id(x) -> [x]
  | Fun(x, y) -> [x] @ (get_ids_expr y)
  | Binop(e1, _, e2) -> (get_ids_expr e1) @ (get_ids_expr e2)
  | Call(id, elist) -> 
  let (oldtype, aformals, stmts) =
   if (GlobalMap.mem id genv)
   then (GlobalMap.find id genv)
 in let rstmts = replace_ids_stmts stmts
   RCall(id, elist, rstmts) (*Can't return this-- we need to do it somewhere else... this is getting so messy*)


and get_ids_list (e: expr list): string list =
  match e with 
    [] -> []
  |h :: t -> get_ids_expr h @ get_ids_list t


let rec get_ids_formals(e: string list) =
  match e with 
    [] -> []
  |h :: t -> h :: get_ids_formals t

let rec get_all_ids (e: stmt list) (g: genvironment): string list =
  match e with 
  | [] -> []
  | hd :: tl ->
    match hd with
    | Asn(x, y, _) -> [x] @ (get_ids_expr y g) @ get_all_ids tl g
    | Return(x) -> (get_ids_expr x g) @ get_all_ids tl g
    | Expr(x) -> (get_ids_expr x g) @ get_all_ids tl g


let get_all_formals_ids (e: func) (g: genvironment): (string list * string) =
  match e with 
  |Fbody(Fdecl(name, formals), stmts) ->
    let rec dedup = function
      | [] -> []
      | x :: y :: xs when x = y -> y :: dedup xs
      | x :: xs -> x :: dedup xs
    in 
    let ids1 = get_all_ids stmts g
    in let ids2 = get_ids_formals formals
    in (dedup (ids1 @ ids2),name)

let infer (e: Ast.func) (genv : genvironment) : (Ast.afunc * genvironment) =
  print_string "Infer in grail\n";
  let vals, fname = get_all_formals_ids e genv in
  let env = List.fold_left (fun m x -> NameMap.add x (Infer.gen_new_type ()) m) NameMap.empty vals in 
  let genv = GlobalMap.add fname (Infer.gen_new_type (),[],[]) genv in 
  print_string "Just before infer in infer\n";
  Infer.infer_func e env genv

let infer_func (e: Ast.func) (genv :  genvironment): (genvironment * Ast.afunc) = 
  print_string "Infer_func in grail\n";
  let (afunc,genv) = infer e genv in (genv, afunc)


let grail (ast: Ast.afunc list) (input: string) : Ast.afunc list =
  let rec do_program(p: Ast.program) (genv : genvironment) : Ast.afunc list  =   
    print_string "Grailing\n";
    match p with
    [] -> []
    |hd :: tl -> let (genv, afunc) =
                   infer_func hd genv 
      in afunc :: do_program tl genv
  in 
  let genv = GlobalMap.add "print" (TInt, [("x" ,TString)], []) GlobalMap.empty in 
  do_program (parse input) genv

let format_sast_codegen (ast : Ast.afunc) : Ast.sast_afunc = 
  match ast with 
    AFbody(AFdecl(name, aformals, t), astmts) ->
    { typ = t; 
      fname = name;
      formals = aformals;
      body = astmts
    }

(* let compile() = grail [] "main() { print(\"Hello, World!\n\"); }";;
compile();
 *)

(* let compile() = let sast = List.map format_sast_codegen (grail [] "main() { print(\"Hello, World!\n\"); }") in
  let m = Codegen.translate sast in
  Llvm_analysis.assert_valid_module m; print_string (Llvm.string_of_llmodule m) ;;
compile();
 *)

(*Interpreter for debugging purposes*)
let rec interpreter (ast: Ast.sast_afunc list) : Ast.sast_afunc list =
  print_string "> ";
    let input = read_line () in
  if input = "exit" then ast
  else
  try
    (*do for func*)
     let pre_ast = List.map format_sast_codegen (grail [] input) in interpreter (pre_ast @ ast) 
  with
  | Failure(msg) ->
    if msg = "lexing: empty token" then [] @ interpreter (ast)
    else (print_endline msg; [] @ interpreter(ast))
  | _ -> print_endline "Error Parsing"; [] @  interpreter (ast)

 
let say() = 
 let str = "Welcome to Grail, the awesomest language!\n"  in 
    print_string str

let rec display (input: Ast.sast_afunc list) : unit = 
   match input with
   [] -> ()
   | h :: t ->
   print_string (string_of_func h); 
   display t;;

say();
let l = interpreter([]) in display l
