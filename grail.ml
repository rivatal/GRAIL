open Ast
open Astutils

let parse (s: string) : Ast.program =
    Parser.program Scanner.token (Lexing.from_string s)

module NameMap = Map.Make(String)
(* module GlobMap = Map.Make(String)
 *)

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

let get_all_formals_ids (e: func): string list =
    match e with 
        |Fbody(Fdecl(name, formals), stmts) ->
        let rec dedup = function
            | [] -> []
            | x :: y :: xs when x = y -> y :: dedup xs
            | x :: xs -> x :: dedup xs
        in 
        let ids1 = get_all_ids stmts 
        in List.iter (fun x ->print_string x) ids1;
        let ids2 = get_ids_formals formals
        in List.iter (fun x ->print_string x) ids2;
        dedup (ids1 @ ids2 @ [name])


let infer (e: Ast.func) : Ast.afunc =
     let vals = get_all_formals_ids e in
	 let env = List.fold_left (fun m x -> NameMap.add x (Infer.gen_new_type ()) m) NameMap.empty vals in
     Infer.infer_func e env



let infer_func (e: Ast.func) : unit = 
    let afunc = infer e in
    match afunc with
    AFbody(AFdecl(name, formals, t), astmts) -> 
      print_endline ("Function " ^ name ^ " " ^ string_of_type t);
          List.iter (fun a -> (print_endline (string_of_type (a)))) formals;
          List.iter (fun a -> (print_endline (string_of_type (Infer.type_of_stmt a)))) astmts 
    
let rec grail () : unit =
  print_string "> ";
    let input = read_line () in
  if input = "" then () else
  try
    (*do for func*)
    let rec do_program(p: Ast.program) =   
        match p with
        [] -> ()
        |hd :: tl -> infer_func hd; do_program tl
    in do_program (parse input);
    grail ()
  with
  | Failure(msg) ->
    if msg = "lexing: empty token" then grail ()
    else print_endline msg; grail ()
  | _ -> print_endline "Error Parsing"; grail ()

let say() = 
 let str = "Welcome to Grail"  in 
    print_string str; 
;;

say();
grail();