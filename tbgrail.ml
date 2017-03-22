(* GRAIL *)
open Ast

let parse (s: string) : Ast.expr =
    Parser.main Scanner.token (Lexing.from_string s)

module NameMap = Map.Make(String)

let rec get_ids (e: expr): id list =
    let rec dedup = function
        | [] -> []
        | x :: y :: xs when x = y -> y :: dedup xs
                   | x :: xs -> x :: dedup xs in
    let ids = match e with
           | IntLit(_) | BoolLit(_) | StrLit(_) -> []
           | Id(x) -> [x]
           | Fun(x, y) -> [x] @ (get_ids y)
           | Binop(e1, _, e2) -> (get_ids e1) @ (get_ids e2)
    dedup ids;

let infer (e: Ast.expr): Ast.aexpr =
    let vals = get_ids e in
    let env = List.fold_left (fun m x -> NameMap.add x (Infer.gen_new_type ()) m) NameMap.empty vals in
    Infer.infer env e;

let rec grail () =
    print_string "> ";
    let input = read_line () in
    if input = "" then () else
    try
        let e = parse input in
        let aexpr = infer e in
        print_endline (string_of_type (Infer.type_of aexpr));
        grail ();
    with
        | Failure(msg) ->
                if msg = "lexing: empty token" then grail ()
                else print_endline msg; grail ()
        | _ -> print_endline "Error"; grail ();

let intro () =
    let template = format_of_string "Welcome to the Grail" in
    Printf.printf template;

intro();
grail();
