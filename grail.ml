open Ast
open Astutils

let parse (s: string) : Ast.func =
    Parser.func Scanner.token (Lexing.from_string s)

module NameMap = Map.Make(String)

let rec get_ids_list(e: expr list) =
  [] -> []
  |h :: t -> get_ids h :: get_ids_list t

let rec get_ids (e: expr): string list =
    match e with
           | IntLit(_) | BoolLit(_) | StrLit(_) -> []
           | Id(x) -> [x]
           | Fun(x, y) -> [x] @ (get_ids y)
           | Binop(e1, _, e2) -> (get_ids e1) @ (get_ids e2)

let rec get_all_ids (e: stmt list): string list =
        match e with 
        | [] -> []
        | hd :: tl -> 
                match hd with
                | Asn(x, y, _) -> [x] @ (get_ids y) @ get_all_ids tl 

let get_all_formals_ids (e: func): string list =
  let formals =
    match FBody(FDecl(name, formals), stmts) -> 
    let rec dedup = function
        | [] -> []
        | x :: y :: xs when x = y -> y :: dedup xs
        | x :: xs -> x :: dedup xs
    and let ids1 = get_all_ids stmts
    and let ids2 = get_ids_list formals
    and let ids3 = get_ids name
  in dedup (ids1 @ ids2 @ ids3)


let infer (e: Ast.func) : Ast.afunc =
	 let vals = get_all_formals_ids e in
	 let env = List.fold_left (fun m x -> NameMap.add x (Infer.gen_new_type ()) m) NameMap.empty vals in
	 Infer.infer_func env e

let rec grail () =
  print_string "> ";
    let input = read_line () in
    print_string input;
  if input = "" then () else
  try
    let e = parse input in 
    let afunc = infer e in
    let print_func =
    match afunc with
     AFBody(AFDecl(name, formals, t), astmts) -> 
     print_endline ("Function " ^ name ^ " " ^ string_of_type t);
     List.iter (fun a -> (print_endline (string_of_type (Infer.type_of a)))) formals; 
     List.iter (fun a -> (print_endline (string_of_type (Infer.type_of_stmt a)))) astmts;
    grail ();
  with
  | Failure(msg) ->
    if msg = "lexing: empty token" then grail ()
    else print_endline msg; grail ()
  | _ -> print_endline "Error"; grail ()

let say() = 
 let str = "Welcome to Grail"  in 
    print_string str; 
;;

say();
grail();
