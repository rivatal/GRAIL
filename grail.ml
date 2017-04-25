open Ast
open Astutils

module NameMap = Map.Make(String)
type environment = primitiveType NameMap.t
module GlobalMap = Map.Make(String)
type genvironment = (primitiveType * (string * primitiveType) list * stmt list) GlobalMap.t
let callstack = Stack.create()


let parse (s: string) : Ast.program =
  Parser.program Scanner.token (Lexing.from_string s)

(*Extra checking functions*)
let checknooverload (e: Ast.func) (genv : genvironment) : unit =
  match e with
    Fbody(Fdecl(name, _), _) ->
    if(GlobalMap.mem name genv) 
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
    if (GlobalMap.mem id genv)
    then (GlobalMap.find id genv)
    else (raise (failwith ("function " ^ id ^ " not defined (grail.ml, get_ids_expr)")))
  in match f with
    (_, aformals, _) -> aformals

(*Add in the formals for called functions, but mapped to the function names so they can only be used in that call.*)
let rec get_ids_expr (e: expr) (genv: genvironment): string list =
  match e with
(*   | IntLit(_) | BoolLit(_) | StrLit(_) | FloatLit(_) | List(_) | Record(_) | Dot(_) -> []
  | Id(x) -> []
  | Binop(e1, _, e2) -> []
  | Unop(_,_) -> []
  | Item(_,_) -> [] *)
  | Call(id, elist) ->  
    Stack.push id callstack;
    let aformals = get_func_if_def id genv in
    let newformals = mapformals id aformals in
    ignore(Stack.pop callstack);
    newformals
  | _ -> []

let rec get_all_ids_stmts (e: stmt list) (g: genvironment): string list =
  match e with 
  | [] -> []
  | hd :: tl ->
    match hd with
    | Asn(x, _, _) -> [(mapid x)] @ get_all_ids_stmts tl g
    | Return(x) -> (get_ids_expr x g) @ get_all_ids_stmts tl g
    | Expr(x) -> (get_ids_expr x g) @ get_all_ids_stmts tl g
    | If(x, y, z) -> (get_all_ids_stmts y g) @ (get_all_ids_stmts z g) @ get_all_ids_stmts tl g
    | For(x, _, _, y) -> (get_all_ids_stmts [x] g) @ (get_all_ids_stmts y g) @ get_all_ids_stmts tl g

let rec dedup = function
  | [] -> []
  | x :: y :: xs when x = y -> y :: dedup xs
  | x :: xs -> x :: dedup xs

let infer (e: Ast.func) (genv : genvironment) : (Ast.afunc * genvironment) =
  checknooverload e genv; 
  match e with 
  |Fbody(Fdecl(name, formals), stmts) ->
    Stack.push name callstack;
    let ids1 = get_all_ids_stmts stmts genv
    in let env = List.fold_left (fun m x -> NameMap.add x (Infer.gen_new_void ()) m) NameMap.empty ids1
    in let ids2 = get_ids_formals formals
    in let env = List.fold_left (fun m x -> NameMap.add x (Infer.gen_new_type ()) m) env ids2 
    in ignore(Stack.pop callstack);
    let genv = GlobalMap.add name (Infer.gen_new_type (),[],[]) genv in
    Infer.infer_func (env, genv, []) e 

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
   let compile() = let sast = List.map format_sast_codegen (grail [] "main() { print(\"hello world\"); x = 2; y = x; z = y;}") in
    let m = Codegen.translate sast in
    Llvm_analysis.assert_valid_module m; print_string (Llvm.string_of_llmodule m) ;;
    compile(); 