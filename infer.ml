(* https://github.com/prakhar1989/type-inference/blob/master/infer.ml *)
(* NTS-- formals need prefixes*)
(*If the function type is different from the returned function type and not in a good way...*)
open Ast
open Astutils   

module NameMap = Map.Make(String)
type environment = primitiveType NameMap.t
module GlobalMap = Map.Make(String)
                    (*type, formals and types list, stmt list*)
type genvironment = (primitiveType * (string * primitiveType) list * stmt list) GlobalMap.t

(* Unknown type,  resolved type. eg.[(T, TInt); (U, TBool)] *)
type substitutions = (id * primitiveType) list
let type_variable = ref (Char.code 'a')
let name_variable = ref (Char.code 'a')

(* generates a new unknown type placeholder.
 *    returns T(string) of the generated alphabet *)
let gen_new_type () =
  let c1 = !type_variable in
  incr type_variable; T(Char.escaped (Char.chr c1))

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

let rec replace_ids_func (f: afunc) =
  match f with
  Fbody(_, stmts) ->



(* Group of functions dealing with annotation:
stmt, list, type of; expr, list, type of
*)
let rec annotate_stmt (e: stmt) (env: environment) (genv: genvironment) : astmt =
(*   ignore(print_string ("Annotating..." ^ string_of_ustmt e));
 *)  match e with
  | Asn(id, expr, switch) -> 
    let aexpr = annotate_expr expr env genv
    and t = 
      if NameMap.mem id env
      then NameMap.find id env
      else raise (failwith (id ^ " not defined in annotate stmt"))
    in AAsn(id, aexpr, switch, t)
  | Return(expr) ->
    let aexpr = annotate_expr expr env genv in AReturn(aexpr, gen_new_type())
  | Expr(expr) -> 
    let aexpr = annotate_expr expr env genv in AExpr(aexpr, gen_new_type()) 
and annotate_stmt_list(st : stmt list ) (env : environment) (genv : genvironment) : astmt list =
  match st with 
  | [] -> []
  | hd :: tl -> (annotate_stmt hd env genv) :: (annotate_stmt_list  tl env genv)
and type_of_stmt (a: astmt): primitiveType = 
  match a with
  | AAsn(_, _, _, t) -> t
  | AReturn(_, t) -> t
  | AExpr(_, t) -> t 

and annotate_expr (e: expr) (env: environment) (genv : genvironment): aexpr =
  match e with
  | IntLit(n) -> AIntLit(n, TInt)
  | BoolLit(b) -> ABoolLit(b, TBool)
  | StrLit(s) -> AStrLit(s,TString)
  | Id(x) -> if NameMap.mem x env
    then AId(x, NameMap.find x env)
    else raise (failwith (x ^ " not defined in annotate expr"))
  | Binop(e1, op, e2) ->
    let et1 = annotate_expr e1 env genv
    and et2 = annotate_expr e2 env genv
    and new_type = gen_new_type () in
    ABinop(et1, op, et2, new_type)
  | Call(id, elist) ->    (*Function calls derive their type from the function declaration*)
    print_string "I'm matching a Call!";

(*    let (oldtype, aformals, stmts) =
      if GlobalMap.mem id genv
      then GlobalMap.find id genv
      else raise (failwith "function not defined")
    in let forms = assign_formals (List.combine aformals elist)
    in let astmts = annotate_stmtlist stmts env genv
 *)let (oldtype, aformals, stmts) =
   if (GlobalMap.mem id genv)
      then (GlobalMap.find id genv)
      else (raise (failwith "function not defined")) in
      let assignments = assign_formals (List.combine aformals elist) in
      let(_,env,genv) = (infer_stmt_list env genv assignments) in
      let (astmts, _, _) = (infer_stmt_list env genv stmts) in
      let t = get_return_type astmts in 
    ACall(id, astmts, t) (*Is it a problem that we never return genv?*)
and annotate_expr_list(e : expr list ) (env : environment) (genv : genvironment) : aexpr list =
  match e with 
  | [] -> []
  | hd :: tl -> ((annotate_expr hd env genv) :: (annotate_expr_list tl env genv ))
and type_of (ae: aexpr): primitiveType =  
  match ae with
  | AIntLit(_, t) | ABoolLit(_, t) | AStrLit(_,t) -> t
  | AId(_, t) -> t
  | ABinop(_, _, _, t) -> t
  | AFun(_, _, t) -> t
  | ACall(_, _, t) -> t

(*we need a system to figure out which binds stricter?*)
(*  and integrate_formals (comblist): id * aexpr list = 
  match oldlist with:
  [] -> []
  ((x, t1), t2) :: t -> [(x, t2)] @ integrate_formals t *)
(*Is this the right order?*)
 
and assign_formals (asnlist: ((id * primitiveType) * expr) list) : stmt list =
  print_string "Assigning formals";
  print_formals asnlist;
  match asnlist with
  [] -> []
  |h :: t ->     (*_ is the old "bad" type*)
  match h with
  ((x, _), e) -> [Asn(x, e, false)] @ assign_formals t

and print_formals (asnlist) =
  match asnlist with
  [] -> ()
  |h :: t ->
  match h with
  ((a, _), c) ->
  print_string (a ^ " " ^ (string_of_expr c))

(*Group of functions dealing with collection:
  collect_stmt and collect_stmt list, collect_expr and "" list
*)
and collect_stmt (a: astmt) : (primitiveType * primitiveType) list =
  match a with
  | AAsn(id, aexpr, switch, t) ->
    collect_expr aexpr @ [(type_of aexpr , t)]
  | AReturn(aexpr, t) ->
    collect_expr aexpr @ [(type_of aexpr , t)]
  | AExpr(aexpr, t) ->
    collect_expr aexpr @ [(type_of aexpr , t)]
and collect_stmt_list (astlist: astmt list) : (primitiveType * primitiveType) list = 
  match astlist with 
  | [] -> []
  | hd :: tl -> (collect_stmt hd) @ collect_stmt_list tl 

and collect_expr (ae: aexpr) : (primitiveType * primitiveType) list =
  match ae with
  | AIntLit(_) | ABoolLit(_) | AStrLit(_) -> []  (* no constraints to impose on literals *)
  | AId(_) -> []                   (* single occurence of val gives us no info *)
  | ABinop(ae1, op, ae2, t) ->
    let et1 = type_of ae1 and et2 = type_of ae2 in
    (* impose constraints based on binary operator *)
    let opc = match op with
      | Add | Mult -> [(et1, TInt); (et2, TInt); (t, TInt)]
      (* we return et1, et2 since these are generic operators *)
      | Greater | Less | Geq | Leq | Neq -> [(et1, et2); (t, TBool)]
      | And | Or -> [(et1, TBool); (et2, TBool); (t, TBool)]
    in
    (collect_expr ae1) @ (collect_expr ae2) @ opc (*opc appended at the rightmost since we apply substitutions right to left *)
  | ACall(id, astmts, t) -> [(t, t)]

and collect_expr_list (ae: aexpr list) : (primitiveType * primitiveType) list =
  match ae with 
  | [] -> []
  | hd :: tl -> (collect_expr hd) @ (collect_expr_list tl)

(*Collection of functions dealing with unify: *)
and unify (constraints: (primitiveType * primitiveType) list) : substitutions =
  match constraints with
  | [] -> []
  | (x, y) :: xs ->
    (* generate substitutions of the rest of the list *)
    let t2 = unify xs in
    (* resolve the LHS and RHS of the constraints from the previous substitutions *)
    let t1 = unify_one (apply t2 x) (apply t2 y) in
    t1 @ t2
and unify_one (t1: primitiveType) (t2: primitiveType) : substitutions =
  match t1, t2 with
  | TInt, TInt | TBool, TBool | TString, TString -> []
  | T(x), z | z, T(x) -> [(x, z)]
  | _ -> raise (failwith "mismatched types")

  (* This case is particularly useful when you are calling a function that returns a function *)
(*   | TFun(a, b), TFun(x, y) -> unify [(a, x); (b, y)]
 *) 
and substitute (u: primitiveType) (x: id) (t: primitiveType) : primitiveType =
  match t with
  | TInt | TBool | TString -> t
  | T(c) -> if c = x then u else t
  | TFun(t1, t2) -> TFun(substitute u x t1, substitute u x t2)
and apply (subs: substitutions) (t: primitiveType) : primitiveType =
  List.fold_right (fun (x, u) t -> substitute u x t) subs t

(*Used in final application of substitutions*) 
and apply_stmt (subs: substitutions) (a: astmt): astmt = 
  match a with
  | AAsn(id, aexpr, switch, t) -> 
    AAsn(id, apply_expr subs aexpr, switch, apply subs t) 
  | AReturn(aexpr, t) ->
    AReturn(apply_expr subs aexpr, apply subs t) 
  | AExpr(aexpr, t) -> 
    AExpr(apply_expr subs aexpr, apply subs t) 
and apply_stmt_list (subs:substitutions) (astlist : astmt list) : astmt list = 
  match astlist with 
  | [] -> []
  | hd :: tl -> (apply_stmt subs hd) ::  apply_stmt_list subs tl
and apply_expr (subs: substitutions) (ae: aexpr): aexpr =
  match ae with
  | ABoolLit(b, t) -> ABoolLit(b, apply subs t)
  | AIntLit(n, t) -> AIntLit(n, apply subs t)
  | AStrLit(s,t) -> AStrLit(s, apply subs t)
  | AId(s, t) -> AId(s, apply subs t)
  | ABinop(e1, op, e2, t) -> ABinop(apply_expr subs e1, op, apply_expr subs e2, apply subs t)
  | AFun(id, e, t) -> AFun(id, apply_expr subs e, apply subs t)
  | ACall(id, astmts, t) -> ACall(id, astmts, apply subs t)
and apply_expr_list (ae: aexpr list) (subs: substitutions) : aexpr list =
  match ae with 
  | [] -> []
  | hd :: tl -> (apply_expr subs hd) :: apply_expr_list tl subs 

(*Functions dealing with updating the map:*)
and update_map (alist : astmt list) (env: environment) : environment =
  match alist with
  | [] -> env
  | hd :: tl -> 
match hd with
  |AAsn(id, aexpr, _, t) ->
    let env = NameMap.add id t env
      in 
      let env = update_expr_map aexpr env 
      in update_map tl env
  |AReturn(aexpr,t) -> 
      let env = update_expr_map aexpr env     
      in update_map tl env
  |AExpr(aexpr, t) -> 
      let env = update_expr_map aexpr env     
      in update_map tl env

and update_expr_map aexpr env = 
      match aexpr with
      | AIntLit(_,_) | ABoolLit(_,_) | AStrLit(_,_) -> env
      | AId(s, t) ->
        let env = NameMap.add s t env in 
        env
      | AFun(id, _, t) -> 
        NameMap.add id t env
      | ABinop(et1, op, et2, t) -> 
        let env = update_expr_map et1 env
        in update_expr_map et2 env
      | ACall(id, astmts, t) -> let env = NameMap.add id t env in env
and update_expr_map_list (ae: aexpr list) (env: environment): environment =
      match ae with
        [] -> env
      |hd :: tl -> let env = update_expr_map hd env in update_expr_map_list tl env
    


(*Checks that return statements are consistent and returns the type*)
and grab_returns (r: astmt list) : primitiveType list =
  match r with
  | [] -> []
  | h :: tail -> 
    match h with
    |AReturn(_, t) ->
      t :: grab_returns tail
    | _ -> grab_returns tail
and get_return_type(r: astmt list) : primitiveType =
  let returns = grab_returns r in
  let rec find_type l =
    match l with
    | [] -> TVoid
    | [t] -> t 
    | x :: y :: tail -> 
      if x = y
      then find_type (y :: tail)
      else raise (failwith "mismatched returns")
  in find_type returns

(*Overall inference functions:*)
and infer_formals (f: string list) (env: environment):  (string * primitiveType) list=
  match f with
  |[] -> []
  | h :: tail -> 
    let t = if NameMap.mem h env
      then ( 
        (*                 print_endline ("\n" ^ h ^ "->" ^ (string_of_type (NameMap.find h env))); *)
        NameMap.find h env )
      else raise (failwith "formal not used") in (h,t) :: infer_formals tail env
and infer_func (f: func) (env: environment) (genv : genvironment) :  (afunc * genvironment)  =
  print_string "Inferring...\n";
  match f with
  |Fbody(decl, infstmts) ->  
    let (infastmts, env, genv) = 
      (infer_stmt_list env genv infstmts)
    in let ret_type =
         get_return_type infastmts            
    in  
    match decl with
    |Fdecl(name, formals) ->           (*annotate the formals*)
      if GlobalMap.mem name genv
      then 
        let aformals = infer_formals formals env in   
        let genv = GlobalMap.add name (ret_type, aformals, infstmts) genv in 
        (AFbody(AFdecl(name, aformals, ret_type), infastmts),genv)
      else raise (failwith "function not defined")
and infer_stmt_list (env: environment) (genv : genvironment) (e: stmt list) : (astmt list * environment * genvironment) =
  print_string "Infer stmt list...\n"; 
  List.iter (fun a -> (print_endline (string_of_ustmt a))) e;
  let annotated_stmtlist = annotate_stmt_list e env genv in
  let constraints =
    collect_stmt_list annotated_stmtlist in 
  let subs = unify constraints in
  type_variable := (Char.code 'a');   (* reset the type counter after completing inference-- ??actually *)
  let retlist = apply_stmt_list subs annotated_stmtlist
  in let env = update_map retlist env 
  in (retlist,env,genv)
and infer_expr_list (env: environment) (genv : genvironment) (e: expr list): (aexpr list * environment * genvironment)  =
  print_string "Infer expr list...";
  let annotated_exprlist = annotate_expr_list e env genv in
  let constraints = collect_expr_list annotated_exprlist in
  let subs = unify constraints in
  type_variable := (Char.code 'a');
  let retlist = apply_expr_list annotated_exprlist subs
in 
  let env = update_expr_map_list retlist env in 
  (retlist,env,genv)














(* let infer_call (c: expr) (env: environment) (genv: genvironment): aexpr =
  match c with
  Call(id, aelist) ->
  let stmts = GlobalMap.find id in
  let t = infer_func (Fbody(Fdecl(id, aelist), stmts)) env genv in
  ACall(id, annotate_expr elist, t) *)


(* let print_constraints (x,y) =
  print_endline (string_of_type(x) ^ ":" ^ string_of_type(y)) 
 *)