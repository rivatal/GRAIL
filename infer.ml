(* https://github.com/prakhar1989/type-inference/blob/master/infer.ml *)
open Ast
open Astutils   

module NameMap = Map.Make(String)
type environment = primitiveType NameMap.t
module GlobalMap = Map.Make(String)
                    (*type, formals and types list, stmt list*)
type genvironment = (primitiveType * (string * primitiveType) list * stmt list) GlobalMap.t
let callstack = Stack.create()


(* Unknown type,  resolved type. eg.[(T, TInt); (U, TBool)] *)
type substitutions = (id * primitiveType) list
let type_variable = ref (Char.code 'a')

(* generates a new unknown type placeholder.
 *    returns T(string) of the generated alphabet *)
let gen_new_type () =
  let c1 = !type_variable in
  incr type_variable; 
  T(Char.escaped (Char.chr c1))

let mapidwith (fname: string )(id: string) : string =
  (fname ^ "#" ^ id)

let mapid (id: string) : string =
  let fname = Stack.top callstack in
  (mapidwith fname id)

let rec findinmap(id: string) (env: environment): primitiveType =
  let curr = Stack.copy callstack in
  let rec fold (myl: string list) =          (*First fold the call stack into a list for searching*)
    if Stack.length curr <= 0 then (List.rev myl)
    else 
      let myl = Stack.pop curr :: myl in fold myl
  in let m = fold []
  in let rec find(l: string list) (id: string) =
    match l with
    [] -> raise (failwith (id ^ " not defined."))
    |h :: t ->
    if (NameMap.mem (mapidwith h id) env)  
    then (NameMap.find (mapidwith h id) env)
    else (find t id)
  in find m id

(* Group of functions dealing with annotation:
stmt, list, type of; expr, list, type of
*)
let rec infer_stmt (e: stmt) (env: environment) (genv: genvironment) : astmt =
 match e with
  | Asn(id, expr, switch) -> 
    let allexpr = infer_expr env genv expr in 
    (match allexpr with
    (r, _, _) ->
    AAsn(id, r, switch))
  | Return(expr) ->
    let allexpr = infer_expr env genv expr in 
    (match allexpr with
    (r, _, _) ->
    AReturn(r, type_of r))
  | Expr(expr) -> 
    let allexpr = infer_expr env genv expr in 
    match allexpr with
    (aexpr, _, _) ->
    AExpr(aexpr) 

and annotate_expr (e: expr) (env: environment) (genv : genvironment): aexpr =
(*   ignore(print_string "annotate_expr");
 *)  
 match e with
  | IntLit(n) -> AIntLit(n, TInt)
  | BoolLit(b) -> ABoolLit(b, TBool)
  | StrLit(s) -> AStrLit(s,TString)
  | FloatLit(f) -> AFloatLit(f, TFloat)
  | CharLit(c) -> ACharLit(c, TChar)
  | Id(x) -> 
      let t = findinmap x env in
      AId(x, t)
  | Binop(e1, op, e2) ->
    let et1 = annotate_expr e1 env genv
    and et2 = annotate_expr e2 env genv
    and new_type = gen_new_type () in
    ABinop(et1, op, et2, new_type)
  | Call(id, elist) ->    (*Function calls derive their type from the function declaration*)
    Stack.push id callstack;
  let (oldtype, aformals, stmts) =
      if (GlobalMap.mem id genv)
      then (GlobalMap.find id genv)
      else (raise (failwith "function not defined")) in
      let assignments = assign_formals (List.combine aformals elist) id in
      let(_,env,genv) = (infer_stmt_list env genv assignments) in
      let (astmts, _, _) = (infer_stmt_list env genv stmts) in
      let t = get_return_type astmts in 
      Stack.pop callstack;
    ACall(id, astmts, t) (*Is it a problem that we never return genv?*)
and type_of (ae: aexpr): primitiveType =  
  match ae with
  | AIntLit(_, t) | ABoolLit(_, t) | AStrLit(_,t) | AFloatLit(_, t) | ACharLit(_,t) -> t
  | AId(_, t) -> t
  | ABinop(_, _, _, t) -> t
  | ACall(_, _, t) -> t

and assign_formals (asnlist: ((id * primitiveType) * expr) list) (id: string): stmt list =
  let rec helper asnlist id = 
    match asnlist with
  [] -> ignore(Stack.pop callstack); []
  |h :: t ->     (*_ is the old "bad" type*)
  match h with
  ((x, _), e) -> [Asn(x, e, false)] @ helper t id
in 
ignore(Stack.push id callstack); helper asnlist id

and print_formals (asnlist) =
  match asnlist with
  [] -> ()
  |h :: t ->
  match h with
  ((a, _), c) ->
  print_string (a ^ " " ^ (string_of_expr c))

and collect_expr (ae: aexpr) : (primitiveType * primitiveType) list =
(*   ignore(print_string "collecting");
 *)  match ae with
  | AIntLit(_) | ABoolLit(_) | AStrLit(_) | AFloatLit(_) | ACharLit(_)-> []  (* no constraints to impose on literals *)
  | AId(_) -> []                   (* single occurence of val gives us no info *)
  | ABinop(ae1, op, ae2, t) ->
    let et1 = type_of ae1 and et2 = type_of ae2 in
    (* impose constraints based on binary operator *)
    let opc = match op with
      | Add | Mult | Sub | Div -> [(et1, TInt); (et2, TInt); (t, TInt)]
      (* we return et1, et2 since these are generic operators *)
      | Greater | Less | Equal | Geq | Leq | Neq -> [(et1, et2); (t, TBool)]
      | And | Or -> [(et1, TBool); (et2, TBool); (t, TBool)]
      | Fadd | Fsub | Fmult | Fdiv -> [(et1, TFloat); (et2, TFloat); (t, TFloat)]
    in
    (collect_expr ae1) @ (collect_expr ae2) @ opc (*opc appended at the rightmost since we apply substitutions right to left *)
  | ACall(id, astmts, t) -> [(t, t)]

(*Collection of functions dealing with unify: *)
and unify (constraints: (primitiveType * primitiveType) list) : substitutions =
(*   ignore(print_string "unifying");
 *)  match constraints with
  | [] -> []
  | (x, y) :: xs ->
    (* generate substitutions of the rest of the list *)
    let t2 = unify xs in
    (* resolve the LHS and RHS of the constraints from the previous substitutions *)
    let t1 = unify_one (apply t2 x) (apply t2 y) in
    t1 @ t2
and unify_one (t1: primitiveType) (t2: primitiveType) : substitutions =
  match t1, t2 with
  | TInt, TInt | TBool, TBool | TString, TString | TFloat, TFloat -> []
  | T(x), z | z, T(x) -> [(x, z)]
  | _ -> raise (failwith "mismatched types")

  (* This case is particularly useful when you are calling a function that returns a function *)
(*   | TFun(a, b), TFun(x, y) -> unify [(a, x); (b, y)]
 *) 
and substitute (u: primitiveType) (x: id) (t: primitiveType) : primitiveType =
  match t with
  | TInt | TBool | TString | TFloat -> t
  | T(c) -> if c = x then u else t
and apply (subs: substitutions) (t: primitiveType) : primitiveType =
  List.fold_right (fun (x, u) t -> substitute u x t) subs t

(*Used in final application of substitutions*) 
and apply_expr (subs: substitutions) (ae: aexpr): aexpr =
  match ae with
  | ABoolLit(b, t) -> ABoolLit(b, apply subs t)
  | AIntLit(n, t) -> AIntLit(n, apply subs t)
  | AStrLit(s,t) -> AStrLit(s, apply subs t)
  | ACharLit(c,t) -> ACharLit(c, apply subs t)
  | AFloatLit(f, t) -> AFloatLit(f, apply subs t)
  | AId(s, t) -> AId(s, apply subs t)
  | ABinop(e1, op, e2, t) -> ABinop(apply_expr subs e1, op, apply_expr subs e2, apply subs t)
  | ACall(id, astmts, t) -> ACall(id, astmts, apply subs t)

(*Functions dealing with updating the map:*)
and update_map (alist : astmt list) (env: environment) : environment =
  match alist with
  | [] -> env
  | hd :: tl -> 
match hd with
  |AAsn(id, aexpr, _) ->
    let t = type_of aexpr in
      let env = NameMap.add (mapid id) t env
      in let env = update_expr_map aexpr env  (*Redundant?*)
      in update_map tl env
  |AReturn(aexpr, _) -> let env = update_expr_map aexpr env     
      in update_map tl env
  |AExpr(aexpr) -> 
      let env = update_expr_map aexpr env     
      in update_map tl env

and update_map_u (a: astmt) (env: environment) : environment = 
match a with
  |AAsn(id, aexpr, _) ->
    let t = type_of aexpr in
      let env = NameMap.add (mapid id) t env
      in let env = update_expr_map aexpr env  (*Redundant?*)
      in env
  |AReturn(aexpr, _) -> let env = update_expr_map aexpr env     
      in env
  |AExpr(aexpr) -> 
      let env = update_expr_map aexpr env in env     

and update_expr_map aexpr env = 
      match aexpr with
      | AIntLit(_,_) | ABoolLit(_,_) | AStrLit(_,_) | AFloatLit(_,_) | ACharLit(_,_) -> env
      | AId(s, t) ->
        let env = NameMap.add (mapid s) t env in 
        env
      | ABinop(et1, op, et2, t) -> 
        let env = update_expr_map et1 env
        in update_expr_map et2 env
      | ACall(id, astmts, t) -> let env = NameMap.add id t env in env

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
    let fid = (mapid h) in
    let t = if NameMap.mem fid env
      then ( 
        NameMap.find fid env )
      else raise (failwith "formal not used") in (h,t) :: infer_formals tail env
and infer_func (f: func) (env: environment) (genv : genvironment) :  (afunc * genvironment)  =
  match f with
  |Fbody(decl, infstmts) -> 
    ignore(match decl with
    Fdecl(name, _) -> Stack.push name callstack);
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
        ignore(Stack.pop callstack);
(*         type_variable := (Char.code 'a');
 *)        (AFbody(AFdecl(name, aformals, ret_type), infastmts),genv)
      else raise (failwith "function not defined")

and infer_stmt_list (env: environment) (genv : genvironment) (e: stmt list) : (astmt list * environment * genvironment) =
(*   print_string "Inferring:\n";
 *) 
 (*  List.iter (fun a -> (print_endline (string_of_ustmt a))) e;  
 *)  let rec helper (env: environment) (genv : genvironment) (e: stmt list) (a: astmt list) = 
  match e with
  [] -> (a, env, genv)
  |h :: t -> 
  let astmt = infer_stmt h env genv in 
  let env = update_map_u astmt env in
  (helper env genv t ([astmt] @ a))
in helper env genv e []

and infer_expr_list (env: environment) (genv : genvironment) (e: expr list): (aexpr list * environment * genvironment)  =
  let rec helper (env: environment) (genv : genvironment) (e: expr list) (a: aexpr list) = 
    match e with
    [] -> (a, env, genv)
    |h :: t ->
    (let tofexpr = infer_expr env genv h in
    match tofexpr with
    (ret, env, genv) -> helper env genv t ([ret] @ a))
  in helper env genv e []

and infer_expr (env: environment) (genv : genvironment) (e: expr): (aexpr * environment * genvironment)  =
  let annotated_expr = annotate_expr e env genv in
  let constraints = collect_expr annotated_expr in 
    (*List.iter print_constraints constraints;*)
    let subs = unify constraints in
    let ret = apply_expr subs annotated_expr
  in let env = update_expr_map ret env in (ret, env, genv)
