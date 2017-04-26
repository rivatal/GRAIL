(*https://github.com/prakhar1989/type-inference/blob/master/infer.ml*)
(*Assoc Arrays! And fancy subset?*)
open Ast
open Astutils   

module NameMap = Map.Make(String)
module GlobalMap = Map.Make(String)
type environment = primitiveType NameMap.t

(*type, formals and types list, stmt list*)
type genvironment = (primitiveType * (string * primitiveType) list * stmt list) GlobalMap.t
let callstack = Stack.create()
type records = (primitiveType * ((id * primitiveType) list)) list
type allenv = environment * genvironment * records


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
  TVoid (*just chr escaped, no T in the TVoid*)



(*Store variables with function names*)
let mapidwith (fname: string )(id: string) : string =
  (fname ^ "#" ^ id)

let mapid (id: string) : string =
  let fname = Stack.top callstack in
  (mapidwith fname id)

(*Store variables with record names*)
let mapidrec (rname: string) (id: string) : string =
  ignore(print_string ("getting name: " ^ rname ^ ";" ^ id)); 
  let name = (rname ^ ";" ^ id) in name

let rec findinmap(id: string) (env: environment): primitiveType =
  let curr = Stack.copy callstack in
  let myl = Stack.pop curr in
  let mapped = mapidwith myl id in
(*   ignore(print_string (" finding " ^ mapped ^ " ")); *)
  if (NameMap.mem (mapped) env)  
  then (NameMap.find (mapped) env)
  else (raise(failwith(mapped ^ " not defined.")))

let check_asn (a: stmt) : unit =
  (*   print_string "Checking assign";*)  
  match a with
    Asn(_,_,_) -> ()
  |_ -> raise(failwith ((string_of_stmt a) ^ " not an assignment statement."))


(* Group of functions dealing with annotation:
   stmt, list, type of; expr, list, type of
*)
let rec infer_stmt (allenv: allenv) (e: stmt): (allenv * astmt) =
(*   ignore(print_string (" inferring " ^ (string_of_stmt e))); *)
  match e with
  | Asn(id, expr, switch) -> 
    let aexpr = infer_expr allenv expr in 
    (allenv, AAsn(id, aexpr, switch, type_of aexpr))
  | Return(expr) ->
    let aexpr = infer_expr allenv expr in 
    (allenv, AReturn(aexpr, type_of aexpr))
  | Expr(expr) -> 
    let aexpr = infer_expr allenv expr in 
    (allenv, AExpr(aexpr))
  | If(expr, s1, s2) ->
    let conditional = infer_expr allenv expr
    in (check_bool conditional);
    let _, as1 = infer_stmt_list allenv s1  (*y should either be defined outside or throw an error... -> we need to throw an error if assignment changes the type.*)
    in                   (*Make sure second stmts isn't empty? More importantly-- currently this env overwrites. But I think we need to be sure any if assignments are the same? *)
    let _, as2 = infer_stmt_list allenv s2
    in (allenv, AIf(conditional, as1, as2))
  | Break -> allenv, ABreak
  | Continue -> allenv, AContinue
  | While(e1, stmts) ->
    let genv, env, recs = allenv in 
    let ae1 = infer_expr allenv e1 in ignore(check_bool ae1); 
    let (_, astmts) = infer_stmt_list allenv stmts in 
    (allenv, AWhile(ae1, astmts))
  | For(s1, e1, s2, stmts) -> (*Needs some fixing*)
    let genv, env, recs = allenv in 
    let backupenv = env in
    (check_asn s1);
    (check_asn s2);
    let ((env, _, recs), as1) = infer_stmt allenv s1 in 
    let ae1 = infer_expr allenv e1 
    in (check_bool ae1);
    let (_, as2) = (infer_stmt allenv s2) in
    let _, astmts = infer_stmt_list allenv stmts in 
(*     let allenv = backupenv, genv, recs in
 *)    (allenv, AFor(as1, ae1, as2, astmts))

(* f(x){ while(x < 3){ y = 5; };}  *)


and annotate_expr (allenv: allenv) (e: expr) (* (env: environment) *) : aexpr =
(*   ignore(print_string ("annotating " ^ (string_of_expr e)));    *)
  let env, genv, recs = allenv in
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
  | Item(s, e) -> 
    let et1 = annotate_expr allenv e in 
    let typ = findinmap s env in
    (match typ with
      TVoid -> raise (failwith (s ^ " not defined @ 115."))
     |TList(t) -> AItem(s, et1, t)
     | _ -> raise (failwith (s ^ " not a list.")))
  | Binop(e1, op, e2) ->
    let et1 = annotate_expr allenv e1
    and et2 = annotate_expr allenv e2
    and new_type = gen_new_type () in
    ABinop(et1, op, et2, new_type)
  | Unop(uop, e1) ->
    let et1 = annotate_expr allenv e1 and t = gen_new_type() in 
    AUnop(uop, et1, t)
  | Dot(e1, entry) -> 
(*     ignore(print_string "annotating dot"); *)
    let et1 = (annotate_expr allenv e1)
    in let record = 
         (match et1 with
          AId(_, TRec(str, elist)) -> str
          |x -> raise(failwith ((string_of_aexpr x) ^ " not an id.")))
    in let recid = (mapid (mapidrec record entry)) in 
    let typ = 
      if (NameMap.mem recid env)
      then (NameMap.find recid env)
      else (raise (failwith (recid ^ " not defined @ 133")))
    in ADot(et1, entry, typ)
  | List(e) -> 
    let ael = annotate_expr_list allenv e in 
    (match ael with
       [] -> AList(ael, TList(gen_new_type()))
     | head :: _ -> let t = type_of head
       in 
       ignore(check_list_exprs ael);
       ignore(check_list_consistency ael);
       AList(ael, TList(t)))
  | Call(id, elist) ->    (*Function calls derive their type from the function declaration*)
    let ael = annotate_expr_list allenv elist in
      Stack.push id callstack;  
    let (oldtype, aformals, stmts) =
      if (GlobalMap.mem id genv)
      then (GlobalMap.find id genv)
      else (raise (failwith "function not defined @ 147")) in
    let assignments = assign_formals (List.combine aformals elist) id in
    let(allenv, _) = (infer_stmt_list allenv assignments) in
    let (_, astmts) = (infer_stmt_list allenv stmts) in
    let t = get_return_type astmts in 
    ignore(Stack.pop callstack);
    ACall(id, ael, astmts, t) 
| Record(pairlist) -> 
    let rec helper (l : (string * expr) list) =
    match l with
    [] -> []
    |(id, expr) :: tl ->
    (id, (annotate_expr allenv expr)) :: helper tl 
    in let apairlist = helper (List.rev pairlist) in
    ARecord(apairlist, gen_new_rec(apairlist))
   (* type records = (primitiveType * ((id * primitiveType) list)) list *)
 | Graph(elist, tedge) ->
   let atedge = annotate_expr allenv tedge in
   let aelist = annotate_expr_list allenv (elist) in
   let rec splitlists l elist rlist =
   (match l with
   |[] -> (elist, rlist)
   |h :: t -> 
    (match h with 
    |AEdge(a,b,c,d,e) -> splitlists t (AEdge(a,b,c,d,e) :: elist) rlist
    |ARecord(aexprs, typ) ->  splitlists t elist (ARecord(aexprs, typ) :: rlist)
    |AList(somelist, _) -> splitlists (List.hd somelist :: t) elist rlist
    |x -> raise(failwith("error " ^ (string_of_aexpr x) ^ " not a valid graph entry@191")))
    ) 
     (*if there's a list, we've already checked for consistency so you can just sample the first thing*)
   in 
   let elist, rlist = splitlists aelist [] [] in 
   let temptype = type_of atedge in 
   ignore(check_list_consistency (AEdge(ANoexpr(gen_new_void()), Dash, ANoexpr(gen_new_void()), atedge, temptype) :: elist));
   ignore(check_list_consistency (rlist)); 
   AGraph(aelist, atedge, TGraph(type_of (List.hd rlist), temptype))
  (*a. check the list for consistency between nodes and edges. (which could be noexprs or lists themselves, or type of e.)
    b. type of e imposes a constraint on ^ and on the graph type. c-- what if there are no nodes? Graph should be a trec of any, and should be overwritable when the first node comes in.
   Remember, edges have nodes in them.
  *)

 | Noexpr -> ANoexpr(gen_new_type())
 | Edge(e1, op, e2, e3) -> 
   let ae1 = annotate_expr allenv e1 and
       ae2 = annotate_expr allenv e2 and
       ae3 = annotate_expr allenv e3 in 
   AEdge(ae1, op, ae2, ae3, TEdge(type_of ae3))   

and annotate_expr_list (allenv: allenv) (e: expr list): aexpr list =
  let helper e =
    match e with
    [] -> []
    |h :: t ->
      annotate_expr allenv h :: annotate_expr_list allenv t in helper (List.rev e)

and type_of (ae: aexpr): primitiveType =  
  match ae with
  | AIntLit(_, t) | ABoolLit(_, t) | AStrLit(_,t) | AFloatLit(_, t) | ACharLit(_,t) -> t
  | AId(_, t) -> t
  | ABinop(_, _, _, t) -> t
  | AItem(_,_,t) -> t
  | ACall(_, _, _, t) -> t
  | AList(_, t) -> t
  | ARecord(_,t) -> t
  | AEdge(_,_,_,_,t) -> t
  | ADot(_,_,t) -> t
  | AUnop(_,_,t) -> t
  | ANoexpr(t) -> t
  | AGraph(_,_,t) -> t


and gen_new_rec (fieldslist : (id * aexpr) list) : primitiveType =
  let rec helper l : (id * primitiveType) list= 
  (match l with
  [] -> []
  | (id, aexpr) :: tl -> 
  (id, type_of aexpr) :: helper tl
  |_ -> raise(failwith(" not valid a record entry @ 36.")))
  in let fields = helper (List.rev fieldslist)
  in let c1 = !type_variable in
  incr type_variable; 
  TRec(Char.escaped (Char.chr c1), fields)

and check_bool (e: aexpr) : unit =
  (*   print_string "Checking bool"; *)
  if(type_of e != TBool)
  then(raise(failwith ((string_of_aexpr e) ^ " not a boolean.")))
  else ()
and check_list_consistency (e: aexpr list) : unit =
  match e with 
  |x :: y :: t -> 
    let tx = type_of x and ty = type_of y in
     ignore(print_string("matching " ^ string_of_type tx ^ " and " ^ string_of_type ty));
    (match tx, ty with
     | a, T(_) -> check_list_consistency (x :: t)
     | T(_), a -> check_list_consistency (y :: t)
     | a, b -> 
      if(a = b) 
      then(check_list_consistency (y :: t))
      else(raise (failwith "List: mismatched types."))
    )
  |[] | _ -> ()
and check_list_exprs (e: aexpr list) : unit =
  match e with 
    [] -> ()
  |h :: t -> 
    match h with
    | AIntLit(_,_) | ABoolLit(_,_) | AStrLit(_,_) | AFloatLit(_,_) | ACharLit(_,_) | AId(_,_) | AList(_,_) -> check_list_exprs t
    | y -> raise(failwith ((string_of_aexpr y) ^ " does not belong in a list."))

and assign_formals (asnlist: ((id * primitiveType) * expr) list) (id: string): stmt list =
  let rec helper asnlist id = 
    match asnlist with
      [] -> []
    |h :: t ->     (*_ is the old "bad" type*)
      match h with
        ((x, _), e) -> [Asn(x, e, false)] @ helper t id
  in helper asnlist id

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
  | AIntLit(_) | ABoolLit(_) | AStrLit(_) | AFloatLit(_) | ACharLit(_) | ARecord(_,_) | AGraph(_,_,_) -> []  (* no constraints to impose on literals *)
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
      | In -> 
      (match et2 with |TList(x) ->     
              [(et1, x); 
              (et2, TList(gen_new_type())); 
              (t, TBool)])
      | Gadd -> 
      (match et1, et2 with |TGraph(n, e), TRec(_, _) -> [(et2, n); (t, TGraph(et2, e))])     
      | Eadd -> 
      (match et1, et2 with |TGraph(n, e), TEdge(f) -> [(et2, e); (t, TGraph(n, et2))])
     in
    (collect_expr ae1) @ (collect_expr ae2) @ opc (*opc appended at the rightmost since we apply substitutions right to left *)
  | AEdge(ae1, op, ae2, ae3, t) ->
    let et1 = type_of ae1 and et2 = type_of ae2 and et3 = type_of ae3 in 
    let opc = match op with
          | To | From | Dash ->
          (match et1, et2 with
          |TRec(_), TRec(_) -> [(et1, et2)]
          | _ -> raise(failwith("error: " ^ string_of_aexpr ae1 ^ " and " ^ string_of_aexpr ae2 ^ " must be nodes.")))
          | _ -> raise(failwith((string_of_op op) ^ " not an edge operator."))
     in 
     ignore(match et3 with
        | TRec(_) | T(_) -> ()
        | _ -> raise(failwith("error: " ^ string_of_aexpr ae3 ^ " not a record.")));
     (collect_expr ae1) @ (collect_expr ae2) @ opc @ (collect_expr ae3)     
  | ADot(ae1, _, _) -> [(type_of ae1, type_of ae1)]
  | AItem(s, ae1, t) -> collect_expr ae1
  (*    let et1 = type_of ae1 in 
        (match et1 with
        TString -> [(t, TAssoc)]
        |
  *)
  | ACall(id, _, astmts, t) -> []
  | ANoexpr(_) -> []
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
  | TInt, TInt | TBool, TBool | TString, TString | TFloat, TFloat | TVoid, TVoid -> []
  | T(x), z | z, T(x) -> [(x, z)]
  | TList(x), TList(y) -> unify_one x y
  | TGraph(a, b), TGraph(c, d) -> unify_one a c @ unify_one b d
  | TEdge(u), TEdge(v) -> unify_one u v
  | TRec(a, b), TRec(c, d) -> if (c = a)
      then [] 
      else raise (failwith "mismatched types")
  | _ -> raise (failwith "mismatched types")

(* This case is particularly useful when you are calling a function that returns a function *)
(*   | TFun(a, b), TFun(x, y) -> unify [(a, x); (b, y)]
*) 
(*Are we handling lists right?*)
and substitute (u: primitiveType) (x: id) (t: primitiveType) : primitiveType =
  (*   print_string "substituting"; *)  
  match t with
  | TInt | TBool | TString | TFloat | TList(_) | TRec(_,_) | TVoid-> t 
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
  | AGraph(aelist, e, t) -> AGraph(apply_expr_list subs aelist, e, apply subs t) (*no apply on the edge template, right?*)
  | AList(e, t) -> AList(apply_expr_list subs e, apply subs t)
  | ABinop(e1, op, e2, t) -> ABinop(apply_expr subs e1, op, apply_expr subs e2, apply subs t)
  | AUnop(op, e1, t) -> AUnop(op, apply_expr subs e1, apply subs t)
  | ARecord(e1, t) -> ARecord(e1, apply subs t)
  | AItem(s, e1, t) -> AItem(s, apply_expr subs e1, apply subs t)
  | ACall(id, e, astmts, t) -> ACall(id, e, astmts, apply subs t)
  | ADot(id, entry, t) -> ADot(id, entry, apply subs t) (*Am I handling this right?*)
  | AEdge(e1, op, e2, e3, t) -> AEdge(apply_expr subs e1, op, apply_expr subs e2, apply_expr subs e3, apply subs t)
  | ANoexpr(t) -> ANoexpr(t) (*is this okay?*)
and apply_expr_list (subs: substitutions) (ae: aexpr list)  : aexpr list =
  let rec helper (ae: aexpr list) (res: aexpr list) = 
    match ae with
      [] -> List.rev res
    |h :: t ->  helper t (apply_expr subs h :: res) 
  in helper ae []

(*Functions dealing with updating the map:*)
and update_mapl (allenv: allenv) (alist : astmt list): environment * records =
  let rec helper (alist : astmt list) (env: environment) (recs: records): environment * records =
  match alist with
  | [] -> env, recs
  | hd :: tl -> 
    let (env, recs) = update_map allenv hd in 
    helper tl env recs 
  in let env, _, recs = allenv 
  in helper alist env recs

and update_map (allenv: allenv) (a: astmt) : (environment * records) = 
(*    ignore(print_string ("updating map for " ^ (string_of_astmt a)));  *)
  let env, genv, allrecs = allenv in
  match a with
  |AAsn(id, aexpr, _,_) ->
(*     ignore(print_string (" updating " ^ id ^ " with type " ^ (string_of_aexpr aexpr))); *)
    let t = type_of aexpr in
    ignore((check_asn_type env id t));
    let env = NameMap.add (mapid id) t env in
    (update_map_expr id aexpr env), allrecs
  |AReturn(aexpr, _) -> env, allrecs
  |AExpr(aexpr) -> env, allrecs     
  |AIf(_, a1, a2) -> env, allrecs 
  |AFor(_, _, _, _) -> env, allrecs
  |AWhile(_,_) -> env, allrecs

and check_asn_type (env: environment) (id: string) (t: primitiveType) : unit  =
  let otype = 
  if(NameMap.mem (mapid id) env)
  then(NameMap.find (mapid id) env)
  else(TVoid) 
  in 
  match otype with
  |TVoid | T(_) -> ()
  | x -> if(t = x) then(()) else (raise(failwith("error: " ^ id ^ " was defined as " ^ string_of_type x)))

(*honestly is this redundant?*)
  and update_map_expr (id: string) (aexpr: aexpr) (env: environment) : environment = 
    match aexpr with  (*Get all the record stuff in the map with its scoping.*)
     | ARecord(et1, t) -> 
       update_map_derived id t env
     | ACall(id, _, astmts, t) -> 
       update_map_derived id t env
     | _ -> env

 and update_map_derived (id: string) (t: primitiveType) (env: environment) : environment = 
(*     ignore(print_string("update map derived")); *)
    (match t with
    TRec(tname, elist) -> 
      let rec helper l env = 
      (match l with
      |[] -> env
      |(field, fieldtype) :: tail -> 
      let env = NameMap.add (mapid (mapidrec tname field)) fieldtype env in helper tail env)
      in helper elist env   
      |_ -> env)

(*Checks that return statements are consistent and returns the type*)
and grab_returns (r: astmt list) : primitiveType list =
  match r with
  | [] -> []
  | h :: tail -> 
    (match h with
     |AReturn(_, t) ->
       t :: grab_returns tail
     |AIf(_, x, y) ->
       let ifs =  grab_returns x  @ grab_returns y in
       if(ifs != [])
       then(raise(failwith("error-- predicate return")))
       else(grab_returns tail)
     |AFor(_, _, _, y) ->
      let fors = grab_returns y in
      if(fors != [])
      then(raise(failwith("error-- predicate return")))
      else (grab_returns tail)
     |AWhile(_, y) ->
      let whiles = grab_returns y in
      if(whiles != [])
      then(raise(failwith("error-- predicate return")))
      else (grab_returns tail)
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

and infer_func (allenv: allenv) (f: func) :  (afunc * genvironment)  =
  let env, genv, recs = allenv in
  match f with
  |Fbody(decl, stmts) -> 
    ignore(match decl with Fdecl(name, _) -> Stack.push name callstack); (*set scope*)
    let ((_,genv, _), istmts) = infer_stmt_list allenv stmts (*infer the function statments*)
    in let ret_type = get_return_type istmts                       
    in match decl with
    |Fdecl(name, formals) ->           (*add function to globalmap*) 
      if GlobalMap.mem name genv
      then 
        let aformals = infer_formals formals env in   
        let genv = GlobalMap.add name (ret_type, aformals, stmts) genv in 
        (ignore(Stack.pop callstack));
        AFbody(AFdecl(name, aformals, ret_type), istmts),genv
      else raise (failwith "function not defined @ 412")


and infer_stmt_list (allenv: allenv) (e: stmt list) : (allenv * astmt list) =
  let rec helper (allenv: allenv) (a: astmt list) (e: stmt list)  : (allenv * astmt list) = 
    match e with
      [] -> (allenv, List.rev a)
     |head :: tail -> 
      let allenv, astmt = infer_stmt allenv head in 
      let _, genv, _ = allenv in 
      let env, recs = update_map allenv astmt in
      (helper (env, genv, recs) (astmt :: a) tail)
  in helper allenv [] e 


(* and infer_expr_list (env: environment) (genv : genvironment) (e: expr list): (aexpr list * environment * genvironment)  =
  let rec helper (env: environment) (genv : genvironment) (e: expr list) (a: aexpr list) = 
    match e with
      [] -> (a, env, genv)
    |h :: t ->
      (let tofexpr = infer_expr env genv h in
       match tofexpr with
         (ret, env, genv) -> helper env genv t ([ret] @ a))
  in helper env genv e []
 *)
and infer_expr (allenv: allenv) (e: expr): (aexpr)  =
  let env, genv, recs = allenv in 
  let annotated_expr = annotate_expr allenv e in
  let constraints = collect_expr annotated_expr in 
  (*List.iter print_constraints constraints;*)
  let subs = unify constraints in
  (*   ignore(print_string ("at this point")); *)
  let ret = apply_expr subs annotated_expr in ret
  (* in let env = update_map_expr ret env in (ret, env, genv) *)
(*Update Expression Map-- what's up with that?*)                                                        