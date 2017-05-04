open Ast
open Astutils

module NameMap = Map.Make(String)
type environment = primitiveType NameMap.t

type genvironment = (primitiveType * (string * primitiveType) list * stmt list) NameMap.t
let callstack = Stack.create()

let parse (s) : Ast.program =
  Parser.program Scanner.token (s)

(*Extra checking functions*)
let checknooverload (e: Ast.func) (genv : genvironment) : unit =
  match e with
    Fbody(Fdecl(name, _), _) ->
    if(NameMap.mem name genv) 
    then (raise (failwith ("function " ^ name ^ " already defined.")))
    else ()

(*https://www.rosettacode.org/wiki/Sort_using_a_custom_comparator#OCaml*)
let mycmp s1 s2 =
  if String.length s1 <> String.length s2 then
    compare (String.length s2) (String.length s1)
  else
    String.compare (String.lowercase s1) (String.lowercase s2)

let rec checkformals (s: string list) : unit =
  let helper l = 
    match l with 
    |[_] | [] -> ()
    | x :: y :: xs ->  if x = y then raise (failwith ("Error: Shared formal " ^ x)) else checkformals (y :: xs)
  in helper (List.sort mycmp s)

(*Prints an annotated function*)
let print_afunc (afunc: Ast.afunc): unit =
  match afunc with
    AFbody(AFdecl(name, aformals, t), astmts) -> 
    print_endline ("Function " ^ name ^ " " ^ string_of_type t);
    List.iter (fun a -> (print_endline (string_of_tuple (a)))) aformals
(*     List.iter (fun a -> (print_endline (string_of_type (Infer.type_of_stmt a)))) astmts
*)
let rec print_afunc_list (ast: Ast.afunc list): unit =
  match ast with
    [] -> ()
  |hd :: tl ->
    print_afunc hd; 
    print_afunc_list tl

(*Prints the name of an unannotated function*)
let print_func (func: Ast.func): unit =
  match func with
    Fbody(Fdecl(name, formals), astmts) -> 
    print_endline ("Function " ^ name);
    List.iter (fun a -> (print_endline a)) formals;
    List.iter (fun a -> (print_endline (string_of_stmt a))) astmts

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
  checkformals e;
  match e with 
    [] -> []
  |h :: t -> (mapid h) :: get_ids_formals t

let get_func_if_def (id: string) (genv: genvironment)  =
  let f = 
    if (NameMap.mem id genv)
    then (NameMap.find id genv)
    else (raise (failwith ("function " ^ id ^ " not defined (grail.ml, get_ids_expr)")))
  in match f with
    (_, aformals, _) -> aformals

(*Add in the formals for called functions, but mapped to the function names so they can only be used in that call.*)
let rec get_ids_expr (e: expr) (genv: genvironment): string list =
  match e with
  | IntLit(_) | BoolLit(_) | StrLit(_) | FloatLit(_) | Item(_,_) | List(_) | Record(_) |  CharLit(_) | Graph(_) -> []
  | Id(x) -> [mapid x]
  | Dot(e, _)  -> (get_ids_expr e genv)
  | Noexpr -> []
  | Binop(e1, _, e2) -> [] | Edge(_,_,_,_) -> []
  | Unop(_,_) -> []
  | Call(id, elist) ->  
    Stack.push id callstack;
    let aformals = get_func_if_def id genv in
    let newformals = mapformals id aformals in
    ignore(Stack.pop callstack);
    newformals

let rec get_all_ids_stmts (e: stmt list) (g: genvironment): string list =
  match e with 
  | [] -> []
  | hd :: tl ->
    match hd with
    | Asn(x, _, _) -> (get_ids_expr x g) @ get_all_ids_stmts tl g
    | While(_,y) -> (get_all_ids_stmts y g)  @ get_all_ids_stmts tl g
    | Return(x) -> (get_ids_expr x g) @ get_all_ids_stmts tl g
    | Expr(x) -> (get_ids_expr x g) @ get_all_ids_stmts tl g
    | If(x, y, z) -> (get_all_ids_stmts y g) @ (get_all_ids_stmts z g) @ get_all_ids_stmts tl g
    | For(x, _, _, y) -> (get_all_ids_stmts [x] g) @ (get_all_ids_stmts y g) @ get_all_ids_stmts tl g

let rec dedup = function
  | [] -> []
  | x :: y :: xs when x = y -> y :: dedup xs
  | x :: xs -> x :: dedup xs

let infer (e: Ast.func) (genv : genvironment) : (Ast.afunc * genvironment) =
(*   ignore(print_string("first!")); *)
  checknooverload e genv; 
  match e with 
  |Fbody(Fdecl(name, formals), stmts) ->
    Stack.push name callstack;
    let ids1 = get_all_ids_stmts stmts genv
    in let env = List.fold_left (fun m x -> NameMap.add x (Infer.gen_new_type ()) m) NameMap.empty ids1
    in let ids2 = get_ids_formals formals
    in let env = List.fold_left (fun m x -> NameMap.add x (Infer.gen_new_type ()) m) env ids2 
    in ignore(Stack.pop callstack);
    let genv = NameMap.add name (Infer.gen_new_type (),[],[]) genv in
    Infer.infer_func (env, genv, []) e

let infer_func (e: Ast.func) (genv :  genvironment): (genvironment * Ast.afunc) = 
  let (afunc,genv) = infer e genv in (genv, afunc)

let grail (ast: Ast.afunc list) (input) : Ast.afunc list =
  let rec do_program(p: Ast.program) (genv : genvironment) (l : Ast.afunc list) : Ast.afunc list  =   
    match p with
    [] -> List.rev l
    |hd :: tl -> let (genv, afunc) =
                   infer_func hd genv 
    in do_program tl genv (afunc :: l) 
  in 
  let builtins = [("print", (TVoid, [("x", TString)], [])); 
                  ("display", (TVoid, [("x", TGraph(Infer.gen_new_type(), Infer.gen_new_type()))], []))]
  in let rec addbuiltins l genv =
    match l with
    |[] -> genv 
    |(a, b) :: t -> let genv = NameMap.add a b genv in addbuiltins t genv
  in let genv = addbuiltins builtins NameMap.empty 
  in 
  do_program (parse input) genv []

let format_sast_codegen (ast : Ast.afunc) : Ast.sast_afunc = 
  match ast with 
    AFbody(AFdecl(name, aformals, t), astmts) ->
    { typ = t; 
      fname = name;
      formals = aformals;
      body = astmts
    }

(*Interpreter for debugging purposes*)
(* let rec interpreter (ast: Ast.sast_afunc list) : Ast.sast_afunc list =
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
    | _ -> print_endline "Syntax Error"; [] @  interpreter (ast)
 *)

(*     let say() = 
      let str = "Welcome to Grail, the awesomest language!\n"  in 
      print_string str

      let rec display (input: Ast.sast_afunc list) : unit = 
      match input with
      [] -> ()
      | h :: t ->
      print_string (string_of_func h); 
      display t;;

      say();
      let l = interpreter([]) in display l *)
   let compile() = 
   let sast = 
   let file = Lexing.from_channel stdin in
   List.map format_sast_codegen (grail [] file) in
   let m = Codegen.translate sast in
   Llvm_analysis.assert_valid_module m; print_string (Llvm.string_of_llmodule m) ;; 
   compile(); 


(* Top-level of the compiler: scan & parse the input,
   check the resulting AST, generate LLVM IR, and dump the module *)
(*
type action = Ast | LLVM_IR | Compile

let _ =
  let action = if Array.length Sys.argv > 1 then
    List.assoc Sys.argv.(1) [ ("-a", Ast);  (* Print the AST only *)
            ("-l", LLVM_IR);  (* Generate LLVM, don't check *)
            ("-c", Compile) ] (* Generate, check LLVM IR *)
  else Compile in
  let lexbuf = Lexing.from_channel stdin in
  let ast = Parser.program Scanner.token lexbuf in
  Semant.check ast;
  match action with
    (*Ast -> print_string (Ast.string_of_program ast)*)
  | LLVM_IR -> print_string (Llvm.string_of_llmodule (Codegen.translate ast))
  | Compile -> let m = Codegen.translate ast in
    Llvm_analysis.assert_valid_module m;
    print_string (Llvm.string_of_llmodule m) *)

