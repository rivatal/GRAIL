open Ast
open Astutils

module NameMap = Map.Make(String)
module GlobalMap = Map.Make(String)
type genvironment = (primitiveType* primitiveType list) GlobalMap.t

let parse (s: string) : Ast.program =
    Parser.program Scanner.token (Lexing.from_string s)


let rec get_ids (e: expr): string list =
    match e with
           | IntLit(_) | BoolLit(_) | StrLit(_) -> []
           | Id(x) -> [x]
           | Fun(x, y) -> [x] @ (get_ids y)
           | Binop(e1, _, e2) -> (get_ids e1) @ (get_ids e2)
           | Call(id, elist) -> id :: (get_ids_list elist)
    and get_ids_list (e: expr list): string list =
      match e with 
      [] -> []
      |h :: t -> get_ids h @ get_ids_list t


let rec get_ids_formals(e: string list) =
 match e with 
    [] -> []
  |h :: t -> h :: get_ids_formals t

let rec get_all_ids (e: stmt list): string list =
        match e with 
        | [] -> []
        | hd :: tl ->
                match hd with
                | Asn(x, y, _) -> [x] @ (get_ids y) @ get_all_ids tl 
                | Return(x) -> (get_ids x) @ get_all_ids tl
                | Expr(x) -> (get_ids x) @ get_all_ids tl
                

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
     let genv = GlobalMap.add fname (Infer.gen_new_type (),[]) genv in 
     Infer.infer_func e env genv


let print_ufunc (func: Ast.func): unit =
    match func with
    Fbody(Fdecl(name, _), _) -> 
      print_endline ("Function " ^ name ^ " ")


let print_func (afunc: Ast.afunc): unit =
    match afunc with
    AFbody(AFdecl(name, _, formals, t), astmts) -> 
      print_endline ("Function " ^ name ^ " " ^ string_of_type t);
      List.iter (fun a -> (print_endline (string_of_type (a)))) formals;
      List.iter (fun a -> (print_endline (string_of_type (Infer.type_of_stmt a)))) astmts


let infer_func (e: Ast.func) (genv :  genvironment): (genvironment * Ast.afunc) = 
    let (afunc,genv) = infer e genv in
(*     print_func afunc; *)
    (genv, afunc)


let rec print_func_list (ast: Ast.afunc list): unit =
    match ast with
    [] -> ()
    |hd :: tl ->
    print_func hd; 
    print_func_list tl

let grail (ast: Ast.afunc list) (input: string) : Ast.afunc list =
    let rec do_program(p: Ast.program) (genv : genvironment) : Ast.afunc list  =   
        match p with
        [] -> []
        |hd :: tl -> let (genv, afunc) = ignore(print_string "acting on "; print_ufunc hd);
                                        infer_func hd genv 
                                        in afunc :: do_program tl genv
    in 
    let genv = GlobalMap.add "print" (TInt, [TString]) GlobalMap.empty in 
    do_program (parse input) genv

let format_sast_codegen (ast : Ast.afunc) : Ast.sast_afunc = 
    match ast with 
    AFbody(AFdecl(name, formals, aformals, t), astmts) ->
        { typ = t; 
          fname = name;
          formals = List.combine formals aformals;
          body = astmts
        }

        
let say() =  let sast = grail [] "main(a){ return 3; } function(x) { main(); return y; }" in 
    print_string "Got here\n";
(* let compile() = let sast = grail [] "main() { print(\"hello world\"); }" in
  let m = Codegen.translate sast in
  Llvm_analysis.assert_valid_module m; print_string (Llvm.string_of_llmodule m) ;;
compile();
 *)
(*let say() =  let sast = grail [] "function(x) { print(x); }" in 

    let rec formlist l = 
    match l with 
    | [] -> []
    | h :: t -> let x = format_sast_codegen h in 
               print_string "Before print string\n"; print_string (string_of_func x) ; 
                x  :: formlist t in  
        formlist (List.rev sast);
;;

say();*)

(* let sast = grail [] "main() { print(\"hello world\"); }" in
  let m = Codegen.translate sast in
  Llvm_analysis.assert_valid_module m; print_string (Llvm.string_of_llmodule m) 
 *)
(*To run interpreter style, you can call this instead of grail*)
(* let rec interpreter (ast: Ast.afunc list) : Ast.afunc list =
  print_string "> ";
    let input = read_line () in
  if input = "exit" then ast
  else
  try
    (*do for func*)
    let rec do_program(p: Ast.program) (genv : genvironment)  =   
        match p with
        [] -> []
        |hd :: tl -> let (genv, afunc) = infer_func hd genv 
                                        in afunc :: do_program tl genv
        in 
        let pre_ast = do_program (parse input) GlobalMap.empty
        in  interpreter (pre_ast @ ast) 
  with
  | Failure(msg) ->
    if msg = "lexing: empty token" then [] @ interpreter (ast)
    else (print_endline msg; [] @ interpreter(ast))
  | _ -> print_endline "Error Parsing"; [] @  interpreter (ast)
 *)(* 
let say() = 
 let str = "Welcome to Grail, the awesomest language!\n"  in 
    print_string str *)
