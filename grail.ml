open Ast
open Astutils

let parse (s: string) : Ast.stmt =
    Parser.stmt Scanner.token (Lexing.from_string s)

module NameMap = Map.Make(String)

let rec get_ids (e: expr): string list =
    match e with
           | IntLit(_) | BoolLit(_) | StrLit(_) -> []
           | Id(x) -> [x]
           | Fun(x, y) -> [x] @ (get_ids y)
           | Binop(e1, _, e2) -> (get_ids e1) @ (get_ids e2)

let get_all_ids (e: stmt): string list =
    let rec dedup = function
        | [] -> []
        | x :: y :: xs when x = y -> y :: dedup xs
        | x :: xs -> x :: dedup xs
      in let ids = match e with
                | Asn(x, y, _) -> [x] @ (get_ids y) in
      dedup ids

let infer (e: Ast.stmt) : Ast.astmt  =
	 let vals = get_all_ids e in
	 let env = List.fold_left (fun m x -> NameMap.add x (Infer.gen_new_type ()) m) NameMap.empty vals in
	 Infer.infer env e

let rec grail () =
  print_string "> ";
    let input = read_line () in
  if input = "" then () else
  try
    let e = parse input in 
    let astmt = infer e in
    print_endline (string_of_type (Infer.type_of_stmt astmt));
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
