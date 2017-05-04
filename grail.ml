open Ast
open Astutils

module NameMap = Map.Make(String)
type environment = primitiveType NameMap.t

type genvironment = (primitiveType * (string * primitiveType) list * stmt list) NameMap.t

let parse (s) : Ast.program =
  Parser.program Scanner.token (s)

(*https://www.rosettacode.org/wiki/Sort_using_a_custom_comparator#OCaml*)
let mycmp l1 l2 =
  (if String.length l1 <> String.length l2 then
    compare (String.length l2) (String.length l1)
  else
    String.compare (String.lowercase l1) (String.lowercase l2))
(*   |_ -> raise(failwith("formal not a string")) *)

(*Extra checking functions*)
let check_overload (e: Ast.func) (genv : genvironment) : unit =
  match e with
    Fbody(Fdecl(fname, _), _) ->
    if(NameMap.mem fname genv) 
    then (raise (failwith ("function " ^ fname ^ " already defined.")))
    else ()

(*checks for shared formals*)
let check_formals (s: string list) : unit =
  let rec helper l = 
    match l with 
    | x :: y :: xs ->  
    if (x = y) then (raise (failwith ("Error: Shared formal.")))
    else (helper (y :: xs))
    |_ -> ()
  in helper (List.sort mycmp s)

let rec get_ids_formals(e: string list)(f: string) =
  check_formals e;
  match e with 
  [] -> []
  |h :: t ->  
  (map_id_with f h) :: get_ids_formals t f

let infer_func (e: Ast.func) (genv : genvironment) : (Ast.afunc * genvironment) =
  check_overload e genv; 
  match e with 
  |Fbody(Fdecl(fname, formals), stmts) ->
    let ids = get_ids_formals formals fname in 
    let env = List.fold_left (fun m x -> NameMap.add x (Infer.gen_new_type ()) m) NameMap.empty ids in 
    let genv = NameMap.add fname (Infer.gen_new_type (),[],[]) genv in
    Infer.infer_func (env, genv) e

let grail (ast: Ast.afunc list) (input) : Ast.afunc list =
  let rec get_sast(p: Ast.program) (genv : genvironment) (l : Ast.afunc list) : Ast.afunc list  =   
    match p with
    [] -> List.rev l
    |hd :: tl -> let (afunc, genv) =
                   infer_func hd genv 
    in get_sast tl genv (afunc :: l) 
  in 
  let builtins = [("print", (TVoid, [("x", TString)], [])); 
                  ("printint", (TVoid, [("x", TInt)], [])); 
                  ("printfloat", (TVoid, [("x", TFloat)], [])); 
                  ("printbool", (TVoid, [("x", TBool)], [])); 
                  ("printchar", (TVoid, [("x", TChar)], [])); 
                  ("display", (TVoid, [("x", TGraph(Infer.gen_new_type(), Infer.gen_new_type()))], []))]
  in let rec addbuiltins l genv =
    match l with
    |[] -> genv 
    |(a, b) :: t -> let genv = NameMap.add a b genv in addbuiltins t genv
  in let genv = addbuiltins builtins NameMap.empty 
  in 
  get_sast (parse (Lexing.from_string input)) genv []

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
      let l = interpreter([]) in display l *)
      
   let compile() = 
   let sast = 
   let file = Lexing.from_channel stdin in
   List.map format_sast_codegen (grail [] file) in
   let m = Codegen.translate sast in
   Llvm_analysis.assert_valid_module m; print_string (Llvm.string_of_llmodule m) ;; 
   compile(); 
