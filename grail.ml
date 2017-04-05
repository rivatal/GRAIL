open Ast
open Astutils

module NameMap = Map.Make(String)

type environment = primitiveType NameMap.t

module GlobalMap = Map.Make(String)

type genvironment = (primitiveType * (string * primitiveType) list * stmt list) GlobalMap.t
let callstack = Stack.create()

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

let mapid (id: string) : string =
  let fname = Stack.top callstack in
  (fname ^ "#" ^ id)

let rec mapformals (fname: string) (aformals: ((string * primitiveType) list)) : string list =
  match aformals with
  [] -> []
  |h :: t ->
  match h with
  (id, _) ->
  [(mapid id)] @ mapformals fname t

let rec get_ids_formals(e: string list) =
  match e with 
    [] -> []
  |h :: t -> (mapid h) :: get_ids_formals t

let rec get_ids_expr (e: expr) (genv: genvironment): string list =
  match e with
  | IntLit(_) | BoolLit(_) | StrLit(_) | FloatLit(_) -> []
  | Id(x) -> []
  | Binop(e1, _, e2) -> []
  | Call(id, elist) ->  
  Stack.push id callstack;
  let (_, aformals, _) =  
   if (GlobalMap.mem id genv)
   then (GlobalMap.find id genv)
   else (raise (failwith ("function " ^ id ^ " not defined in get_ids_expr"))) in
   let newformals = mapformals id aformals in
   Stack.pop callstack;
(*    print_string "Formals added:\n"; 
   List.iter(fun a -> (print_endline a)) newformals;
 *)   newformals

let rec get_all_ids (e: stmt list) (g: genvironment): string list =
  match e with 
  | [] -> []
  | hd :: tl ->
    match hd with
    | Asn(x, _, _) -> [(mapid x)] @ get_all_ids tl g
    | Return(x) -> (get_ids_expr x g) @ get_all_ids tl g
    | Expr(x) -> (get_ids_expr x g) @ get_all_ids tl g


let get_all_formals_ids (e: func) (g: genvironment): (string list * string) =
  match e with 
  |Fbody(Fdecl(name, formals), stmts) ->
    Stack.push name callstack;
    let rec dedup = function
      | [] -> []
      | x :: y :: xs when x = y -> y :: dedup xs
      | x :: xs -> x :: dedup xs
    in 
    let ids1 = get_all_ids stmts g
    in let ids2 = get_ids_formals formals
    in ignore(Stack.pop callstack);
    (dedup (ids1 @ ids2),name)

let checknooverload (e: Ast.func) (genv : genvironment) : unit =
  match e with
  Fbody(Fdecl(name, _), _) ->
  if(GlobalMap.mem name genv) 
  then (raise (failwith ("function " ^ name ^ " already defined.")))
  else ()

let infer (e: Ast.func) (genv : genvironment) : (Ast.afunc * genvironment) =
  let vals, fname = get_all_formals_ids e genv in
  checknooverload e genv; 
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


(*  let compile() = let sast = List.map format_sast_codegen (grail [] "main() { print(\"hello world\"); }") in
  let m = Codegen.translate sast in
  Llvm_analysis.assert_valid_module m; print_string (Llvm.string_of_llmodule m) ;;
compile(); *)
