(*https://github.com/prakhar1989/type-inference/blob/master/infer.ml*)
(*Assoc Arrays! And fancy subset?*)
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

let gen_new_void () : primitiveType =
  TVoid
(*   let c1 = !type_variable in
     incr type_variable; 
     TVoid(Char.escaped (Char.chr c1))
*)
(*Store variables with function names*)
let mapidwith (fname: string )(id: string) : string =
  (fname ^ "#" ^ id)

let mapid (id: string) : string =
  let fname = Stack.top callstack in
  (mapidwith fname id)

(*Store variables with record names*)
let mapidrec (rname: string) (id: string) : string =
  (rname ^ ";" ^ id)

let rec findinmap(id: string) (env: environment): primitiveType =
  let curr = Stack.copy callstack in
  let rec fold (myl: string list) =          (*First fold the call stack into a list for searching*)
    if Stack.length curr <= 0 then (List.rev myl)
    else 
      let myl = Stack.pop curr :: myl in fold myl
  in let m = fold []
  in let rec find(l: string list) (id: string) = (*Then find the id in the list*)
       match l with
         [] -> gen_new_void()
       |h :: t -> 
         if (NameMap.mem (mapidwith h id) env)  
         then (NameMap.find (mapidwith h id) env)
         else (find t id)
  in find m id

let check_asn (a: stmt) : unit =
  (*   print_string "Checking assign";
  *)  match a with
    Asn(_,_,_) -> ()
  |_ -> raise(failwith ((string_of_stmt a) ^ " not an assignment statement."))


(* Group of functions dealing with annotation:
   stmt, list, type of; expr, list, type of
*)
let rec infer_stmt (e: stmt) (env: environment) (genv: genvironment) : (astmt * environment * genvironment) =
  match e with
  | Asn(id, expr, switch) -> 
    let aexpr, _, _ = infer_expr env genv expr in 
    (AAsn(id, aexpr, switch, type_of aexpr), env, genv)
  | Return(expr) ->
    let aexpr, _, _ = infer_expr env genv expr in 
    (AReturn(aexpr, type_of aexpr), env, genv)
  | Expr(expr) -> 
    let aexpr, _, _ = infer_expr env genv expr in 
    (AExpr(aexpr), env, genv)
  | If(expr, s1, s2) ->
    let conditional, _, _ = infer_expr env genv expr
    in (check_bool conditional);
    let as1, env, genv = infer_stmt_list s1 env genv 
    in                   (*Make sure second stmts isn't empty? More importantly-- currently this env overwrites. But I think we need to be sure any if assignments are the same? *)
    let as2, env, genv = infer_stmt_list s2 env genv 
    in (AIf(conditional, as1, as2), env, genv)
  | Break -> ABreak, env, genv
  | Continue -> AContinue, env, genv
  | For(s1, e1, s2, stmts) ->
    let backupenv = env in
    (check_asn s1);
    (check_asn s2);
    let as1, env, _ = infer_stmt s1 env genv in 
    let ae1, _, _ = infer_expr env genv e1 in 
    (check_bool ae1);
    let as2, _, _ = infer_stmt s2 env genv in
    let astmts, _, _ = infer_stmt_list stmts env genv  in 
    (AFor(as1, ae1, as2, astmts), backupenv, genv)


and annotate_expr (e: expr) (env: environment) (genv : genvironment): aexpr =
(*   ignore(print_string ("annotating " ^ (string_of_expr e)));    *)
  match e with
  | IntLit(n) -> AIntLit(n, TInt)
  | BoolLit(b) -> ABoolLit(b, TBool)
  | StrLit(s) -> AStrLit(s,TString)
  | FloatLit(f) -> AFloatLit(f, TFloat)
  | CharLit(c) -> ACharLit(c, TChar)
  | Id(x) -> 
(*     ignore(print_string ("Finding " ^ x ^ "\n")); *)
    let typ = findinmap x env in 
    (match typ with
     (* TVoid(_) -> raise (failwith (x ^ " not defined @ 109.")) *)
     |t ->  AId(x, typ))
  | Unop(uop, e1) ->
    let et1 = annotate_expr e1 env genv and t = gen_new_type() in AUnop(uop, et1, t)
  | Item(s, e) -> 
    let et1 = annotate_expr e env genv in 
    let typ = findinmap s env in
    (match typ with
       TVoid -> raise (failwith (s ^ " not defined @ 115."))
     |TList(t) -> AItem(s, et1, t)
     | _ -> raise (failwith (s ^ " not a list.")))
  | Binop(e1, op, e2) ->
    let et1 = annotate_expr e1 env genv
    and et2 = annotate_expr e2 env genv
    and new_type = gen_new_type () in
    ABinop(et1, op, et2, new_type)
  | Dot(e1, entry) -> 
    let et1 = (annotate_expr e1 env genv)
    in let record = 
         (match et1 with
          AId(s, t) -> s
          |x -> raise(failwith ((string_of_aexpr x) ^ " not an id.")))
    in let recid = (mapid (mapidrec record entry)) in 
    let typ = 
      if (NameMap.mem recid env)
      then (NameMap.find recid env)
      else (raise (failwith (recid ^ " not defined @ 133")))
    in ADot(et1, entry, typ)
  | List(e) -> 
    let ael = annotate_expr_list e env genv in 
    (match ael with
       [] -> AList(ael, TList(gen_new_type()))
     | head :: _ -> let t = type_of head
       in ignore(check_list_exprs ael);
       AList(ael, TList(t)))
  | Call(id, elist) ->    (*Function calls derive their type from the function declaration*)
    Stack.push id callstack;
    let (oldtype, aformals, stmts) =
      if (GlobalMap.mem id genv)
      then (GlobalMap.find id genv)
      else (raise (failwith "function not defined @ 147")) in
    let assignments = assign_formals (List.combine aformals elist) id in
    let(_,env,genv) = (infer_stmt_list assignments env genv ) in
    let (astmts, _, _) = (infer_stmt_list stmts env genv ) in
    let t = get_return_type astmts in 
    ignore(Stack.pop callstack);
    ACall(id, astmts, t) 
  | Record(pairlist) -> 
    let rec helper pl =
      (match pl with
         [] -> []
       |(id, expr) :: t -> let (inf, _, _) = (infer_stmt (Asn(id, expr, false)) env genv) 
         in inf :: (helper t)) 
    in let astmts = helper pairlist
    in ARecord(astmts, TRec)

and annotate_expr_list (e: expr list) (env: environment) (genv : genvironment): aexpr list =
  let helper e =
    match e with
      [] -> []
    |h :: t ->
      annotate_expr h env genv :: annotate_expr_list t env genv in helper (List.rev e)

and type_of (ae: aexpr): primitiveType =  
  match ae with
  | AIntLit(_, t) | ABoolLit(_, t) | AStrLit(_,t) | AFloatLit(_, t) | ACharLit(_,t) -> t
  | AId(_, t) -> t
  | ABinop(_, _, _, t) -> t
  | AItem(_,_,t) -> t
  | ACall(_, _, t) -> t
  | AList(_, t) -> t
  | ARecord(_,t) -> t
  | ADot(_,_,t) -> t
  | AUnop(_,_,t) -> t


and check_bool (e: aexpr) : unit =
  (*   print_string "Checking bool"; *)
  if(type_of e != TBool)
  then(raise(failwith ((string_of_aexpr e) ^ " not a boolean.")))
  else ()
and check_list_consistency (e: aexpr list) : unit =
  match e with 
  |x :: y :: t -> 
    if ((type_of x) != (type_of y))
    then (raise (failwith "List: mismatched types."))
    else (check_list_consistency (y :: t))
  |[] -> ()
  | _ -> ()
and check_list_exprs (e: aexpr list) : unit =
  match e with 
    [] -> ()
  |h :: t -> 
    match h with
    | AIntLit(_,_) | ABoolLit(_,_) | AStrLit(_,_) | AFloatLit(_,_) | ACharLit(_,_) | AId(_,_) | AList(_,_) -> check_list_exprs t
    | y -> raise(failwith ((string_of_aexpr y) ^ " does not belong in a list."))

(* and assign_records (asnlist: ((id * expr) list) (id: string): stmt list =
   let rec helper asnlist id = 
    match asnlist with
    [] -> []
    |h :: t ->     (*_ is the old "bad" type*)
    match h with
    (x, e) -> [Asn(x, e, false)] @ helper t id
   in helper asnlist id
*)
and assign_formals (asnlist: ((id * primitiveType) * expr) list) (id: string): stmt list =
  let rec helper asnlist id = 
    match asnlist with
      [] -> (* ignore(Stack.pop callstack); *) []
    |h :: t ->     (*_ is the old "bad" type*)
      match h with
        ((x, _), e) -> [Asn(x, e, false)] @ helper t id
  in 
  (* ignore(Stack.push id callstack); *) helper asnlist id

and print_formals (asnlist) =
  match asnlist with  
    [] -> ()
  |h :: t ->
    match h with
      ((a, _), c) ->
      print_string (a ^ " " ^ (string_of_expr c))

and collect_expr (ae: aexpr) : (primitiveType * primitiveType) list =
  (*   ignore(print_string "collecting"); *)
  match ae with
  | AIntLit(_) | ABoolLit(_) | AStrLit(_) | AFloatLit(_) | ACharLit(_) | ARecord(_) -> []  (* no constraints to impose on literals *)
  | AId(_) -> []                   (* single occurence of val gives us no info *)
  | AUnop(uop, ae1, t) ->
    let et1 = type_of ae1 in 
    let opc = match uop with
    | Not -> [(et1, TBool); (t, TBool)]
    | Neg -> [(et1, TInt); (t, TInt)]
  in (collect_expr ae1) @ opc
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
  | ADot(ae1, _, _) -> [(type_of ae1, TRec)]
  | AItem(s, ae1, t) -> collect_expr ae1
  (*    let et1 = type_of ae1 in 
        (match et1 with
        TString -> [(t, TAssoc)]
        |
  *)
  | ACall(id, astmts, t) -> [(t, t)]
  | AList(ael, t) -> 
    let rec helper l = 
      match l with
      |x :: y :: tail -> (type_of x, type_of y) :: helper (y :: tail)
      |[] | _ -> []
    in (helper ael)

(*Collection of functions dealing with unify: *)
and unify (constraints: (primitiveType * primitiveType) list) : substitutions =
  (*     ignore(print_string "unifying"); *)
  match constraints with
  | [] -> []
  | (x, y) :: xs ->
    (* generate substitutions of the rest of the list *)
    let t2 = unify xs in
    (* resolve the LHS and RHS of the constraints from the previous substitutions *)
    let t1 = unify_one (apply t2 x) (apply t2 y) in
    (*     ignore(print_string ("after unify one")); *)
    t1 @ t2
and unify_one (t1: primitiveType) (t2: primitiveType) : substitutions =
  (*     ignore(print_string ((string_of_type t1) ^ "\n"));  
         ignore(print_string ((string_of_type t2) ^ "\n"));   *)
  match t1, t2 with
  | TInt, TInt | TBool, TBool | TString, TString | TFloat, TFloat | TRec, TRec | TVoid, TVoid -> []
  | T(x), z | z, T(x) -> [(x, z)]
  | TList(x), TList(y) -> unify_one x y
  | _ -> raise (failwith "mismatched types")

(* This case is particularly useful when you are calling a function that returns a function *)
(*   | TFun(a, b), TFun(x, y) -> unify [(a, x); (b, y)]
*) 
(*Are we handling lists right?*)
and substitute (u: primitiveType) (x: id) (t: primitiveType) : primitiveType =
  (*   print_string "substituting"; *)  
  match t with
  | TInt | TBool | TString | TFloat | TList(_) | TRec | TVoid-> t 
  | T(c)  -> if c = x then u else t 
and apply (subs: substitutions) (t: primitiveType) : primitiveType =
  List.fold_right (fun (x, u) t -> substitute u x t) subs t

(*Used in final application of substitutions*) 
and apply_expr (subs: substitutions) (ae: aexpr): aexpr =
  (*   ignore(print_string "applying"); *)
  match ae with
  | ABoolLit(b, t) -> ABoolLit(b, apply subs t)
  | AIntLit(n, t) -> AIntLit(n, apply subs t)
  | AStrLit(s,t) -> AStrLit(s, apply subs t)
  | ACharLit(c,t) -> ACharLit(c, apply subs t)
  | AFloatLit(f, t) -> AFloatLit(f, apply subs t)
  | AId(s, t) -> AId(s, apply subs t)
  | AList(e, t) -> AList(apply_expr_list subs e, apply subs t)
  | ABinop(e1, op, e2, t) -> ABinop(apply_expr subs e1, op, apply_expr subs e2, apply subs t)
  | AUnop(op, e1, t) -> AUnop(op, apply_expr subs e1, apply subs t)
  | ARecord(e1, t) -> ARecord(e1, apply subs t)
  | AItem(s, e1, t) -> AItem(s, apply_expr subs e1, apply subs t)
  | ACall(id, astmts, t) -> ACall(id, astmts, apply subs t)
  | ADot(id, entry, t) -> ADot(id, entry, apply subs t) (*Am I handling this right?*)
and apply_expr_list (subs: substitutions) (ae: aexpr list)  : aexpr list =
  let rec helper (ae: aexpr list) (res: aexpr list) = 
    match ae with
      [] -> List.rev res
    |h :: t ->  helper t (apply_expr subs h :: res) 
  in helper ae []

(*Functions dealing with updating the map:*)
and update_mapl (alist : astmt list) (env: environment) : environment =
  match alist with
  | [] -> env
  | hd :: tl -> 
    let env = update_map hd env in update_mapl tl env

and update_map (a: astmt) (env: environment) : environment = 
(*   ignore(print_string ("updating map for " ^ (string_of_astmt a))); *)
  match a with
  |AAsn(id, aexpr, _,_) ->
(*     ignore(print_string (" updating " ^ id ^ " with type " ^ (string_of_aexpr aexpr))); *)
    let t = type_of aexpr in
    let env = NameMap.add (mapid id) t env in
(*     ignore(print_string "here"); *)
    (match aexpr with  (*Get all the record stuff in the map with its scoping.*)
     | ARecord(et1, t) -> 
       let rec helper asns env = 
         (match asns with 
          |[] -> env
          |AAsn(entry, aexpr, _,_) :: tail ->
            let t = type_of aexpr in
            let env = NameMap.add (mapid (mapidrec id entry)) t env in helper tail env)
       in (helper et1 env)
     |_ -> let env = update_map_expr aexpr env in env)  (*Redundant?*)
  |AReturn(aexpr, _) -> let env = update_map_expr aexpr env     
    in env
  |AExpr(aexpr) -> 
    let env = update_map_expr aexpr env in env     
  |AIf(_, a1, a2) ->
    let env = update_mapl a1 env in 
    let env = update_mapl a2 env in env
  |AFor(_, _, _, _) -> env

(*honestly is this redundant?*)
and update_map_expr (aexpr: aexpr) (env: environment) : environment = 
  match aexpr with
  | AIntLit(_,_) | ABoolLit(_,_) | AStrLit(_,_) | AFloatLit(_,_) | ACharLit(_,_) | AList(_,_) -> env
  | AUnop(op, et1, t) -> env
  | ADot(_,_,_) -> env
  | AId(s, t) ->
    (*     let env = NameMap.add (mapid s) t env in  *)
    env
  | ABinop(et1, op, et2, t) -> 
    let env = update_map_expr et1 env
    in update_map_expr et2 env
  | ARecord(et1, t) -> env
  | AItem(s, et1, t) -> let env = update_map_expr et1 env in env (*Is this really necessary? We're not putting calls in arrays, are we?*)
  | ACall(id, astmts, t) -> let env = NameMap.add id t env in env

(*Checks that return statements are consistent and returns the type*)
and grab_returns (r: astmt list) : primitiveType list =
  match r with
  | [] -> []
  | h :: tail -> 
    (match h with
     |AReturn(_, t) ->
       t :: grab_returns tail
     |AIf(_, x, y) ->
       grab_returns x @ grab_returns y @ grab_returns tail
     |AFor(_, _, _, y) ->
       grab_returns y @ grab_returns tail
     | _ -> grab_returns tail)
and get_return_type(r: astmt list) : primitiveType =
  let returns = grab_returns r in
  let rec find_type l : primitiveType =
    match l with
      [] -> gen_new_void()
    | [t] -> t 
    | x :: y :: tail -> 
      if x = y
      then find_type (y :: tail)
      else raise (failwith "mismatched returns")
  in (find_type returns)

(*Overall inference functions:*)
and infer_formals (f: string list) (env: environment):  (string * primitiveType) list=
  (*   ignore(print_string "Inferring formals!"); *)
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
      (infer_stmt_list infstmts env genv)
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
      else raise (failwith "function not defined @ 412")

and infer_stmt_list (e: stmt list) (env: environment) (genv : genvironment)  : (astmt list * environment * genvironment) =
  (*   print_string "Inferring:\n";
  *)  (*  List.iter (fun a -> (print_endline (string_of_ustmt a))) e;  
       *)  
  let rec helper (env: environment) (genv : genvironment) (e: stmt list) (a: astmt list) = 
    match e with
      [] -> (List.rev a, env, genv)
    |h :: t -> 
      let astmt, env, genv = infer_stmt h env genv in 
      let env = update_map astmt env in
      (helper env genv t (astmt :: a))
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
  (*   ignore(print_string ("at this point")); *)
  let ret = apply_expr subs annotated_expr in (ret, env, genv)
(* in let env = update_map_expr ret env in (ret, env, genv) *)
(*Update Expression Map-- what's up with that?*)                                                        