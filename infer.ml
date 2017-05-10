(*https://github.com/prakhar1989/type-inference/blob/master/infer.ml*)
(*Assoc Arrays! And fancy subset?*)
open Ast
open Astutils   

module NameMap = Map.Make(String)
type environment = primitiveType NameMap.t
type genvironment = (primitiveType * (string * primitiveType) list * stmt list) NameMap.t
type recs = primitiveType list 
type funcs = afunc list 
type allenv = environment * genvironment * recs * funcs
(* local, global, records, functions*)
(* Unknown type,  resolved type. eg.[(T, TInt); (U, TBool)] *)
type substitutions = (id * primitiveType) list

let callstack = Stack.create()

let map_id (id: string) : string =
  let fname = Stack.top callstack in
  (map_id_with fname id)

(*One for function templates generated, one for type variables*)
let func_variable = ref 1
let type_variable = ref 1 

(* generates a new unknown type placeholder.
 * returns T(string) of the generated int *)
let gen_new_type () =
  let c1 = !type_variable in
  incr type_variable; 
  T(string_of_int c1)

let get_func_name (id: id) : string =
  let calln = !func_variable in
  incr func_variable;
  map_func_id id (string_of_int   calln) 

let get_id (e: expr) : string =
  match e with
  |Id(str) -> str 
  |_ -> raise(failwith(string_of_expr e ^ " is not an id."))

let get_subtype (t: primitiveType) : primitiveType =
  match t with
  |TList(st) -> st
  | T(_) -> t 
  |x -> raise(failwith("error: " ^ string_of_type x ^ " not iterable."))

let type_of (ae: aexpr): primitiveType =  
  match ae with
  | AIntLit(_, t) | ABoolLit(_, t) | AStrLit(_,t) | AFloatLit(_, t) | ACharLit(_,t) -> t
  | AId(_, t) -> t
  | ABinop(_, _, _, t) -> t
  | AItem(_,_,t) -> t
  | ACall(_, _, _,_, t) -> t
  | AList(_, t) -> t
  | ARecord(_,t) -> t
  | AEdge(_,_,_,_,t) -> t
  | ADot(_,_,t) -> t
  | AUnop(_,_,t) -> t
  | ANoexpr(t) -> t
  | AGraph(_,_,t) -> t

(*Generate unique record type based on fields*)
let gen_new_rec (fieldslist : (id * aexpr) list) : primitiveType =
  let fields = List.map (fun (a, b) -> a, type_of b) fieldslist
  in TRec(gen_new_type(), fields)

let get_rec (recs: recs) (fieldslist: (id * aexpr) list) : primitiveType =
  let rec helper (l : recs) (curr : ((id * primitiveType) list)) (rl : recs) = 
  match l with
  |[] -> let newtype = gen_new_rec(fieldslist) in newtype
  |TRec(name, fl) :: t -> 
  if(fl = curr) 
  then(TRec(name, fl)) 
  else(helper t curr rl)
  |_ -> raise(failwith("error"))
in helper recs (List.map (fun (a, b) -> a, type_of b) fieldslist) recs


(*Ensures an expression is a conditional (e.g. for predicate statements)*)
let check_bool (e: aexpr) : unit =
(*     print_string "Checking bool"; *)
  if(type_of e != TBool)
  then(raise(failwith ((string_of_aexpr e) ^ " not a boolean.")))
  else ()

(*A checking function for something like the first field of a for*)
let check_asn (a: stmt) : unit =
  (*   print_string "Checking assign\n";*)  
  match a with
    Asn(_,_,_) -> ()
  |_ -> raise(failwith ((string_of_stmt a) ^ " not an assignment statement."))

let format_formal (formal: (string * primitiveType) * aexpr) : string * primitiveType =
  match formal with 
  ((x, _), e) -> (x, type_of e)

(* Updates the name map for the formals with the types of the actuals. *)
let update_types_formals (stufflist: ((id * primitiveType) * aexpr) list) (env: environment) (id: string): environment =
  List.fold_left (fun e f -> let id, typ = format_formal f in 
    NameMap.add (map_id id) typ e) env stufflist

(* In graph, the type of the edges can be inferred from the type of the graph *)
let enforce_type (ae: aexpr) (nt: primitiveType): aexpr =  
  match ae with
  | AId(a, t) -> AId(a, nt)
  | ABinop(a,b,c,t) ->  ABinop(a,b,c,nt)
  | AItem(a,b,t) -> AItem(a,b,nt)
  | ACall(a,b,c,d, t) -> ACall(a,b,c,d,nt)
  | AList(a, t) -> AList(a, nt)
  | ARecord(a,t) -> ARecord(a,nt)
  | AEdge(a,b,c,d,t) -> AEdge(a,b,c,d,nt)
  | ADot(a,b,t) -> ADot(a,b,nt)
  | AUnop(a,b,t) -> AUnop(a,b,nt)
  | ANoexpr(t) -> ANoexpr(nt)
  | AGraph(a,b,t) ->  AGraph(a,b,nt)
  | ABoolLit(a,t) -> ABoolLit(a,nt) 
  | ACharLit(a,t) -> ACharLit(a,nt) 
  | AIntLit(a,t) -> AIntLit(a,nt) 
  | AStrLit(a,t) -> AStrLit(a,nt) 
  | AFloatLit(a,t) -> AFloatLit(a,nt) 

(*Comparator used in annotating records.*)
let comp (x: id * expr) (y: id * expr) : int = 
  match x, y with
  |(a,_), (b,_) -> if(a = b) then(0) else(if(a < b) then(-1) else(1))

(*Helper function for annotating records (check for duplicate fields)*)
let rec has_dups l = 
  match l with
  |(a,_) :: (b,c) :: tail -> if(a = b) then(true) else(has_dups((b,c)::tail))
  |[]| _ -> false

(*finds the variable in the map*)
let rec find_in_map(id: string) (env: environment): primitiveType =
   let mapped = map_id id in       (*in astutils*)
   if (NameMap.mem mapped env)  
   then (NameMap.find mapped env)
   else (raise(failwith(mapped ^ " not found@79")))

(* Runs over all the nodes and edges and assigns them the type *)
let enforce_node_consistency (plist: aexpr list) (typ: primitiveType) = 
  let rec helper pl typ = 
  match pl with 
  |[] -> []
  |h :: tl -> 
  let enforced = match type_of h with 
  |TRec(_,_)| T(_) | TVoid  -> h
  |TEdge(_,_,_) -> enforce_type h typ 
  |x -> raise(failwith(string_of_aexpr h ^ " should not be in constructor."))
  in enforced :: helper tl typ 
in helper plist typ

(* Split the graph constructor into two lists based on their types *)
let rec split_types (aelist: aexpr list) : (primitiveType list * primitiveType list) = 
  let rec helper l edgelist nodelist : (primitiveType list * primitiveType list) =
  (match l with 
  |[] -> edgelist, nodelist
  |h :: t ->
  let et1 = type_of h in
  (match et1 with 
   |TRec(_,_) -> helper t edgelist (et1 :: nodelist)
   |TEdge(_,n,_) -> helper t (et1 :: edgelist) (n :: nodelist)
   |T(_) | TVoid -> helper t edgelist nodelist 
   |_ -> raise(failwith(string_of_type et1 ^ " not a graph type."));
  ))
  in (helper aelist [] [])

(*Searches a list of record fields for a particular id and gets its type*)
let rec get_field_type (elist: (id * primitiveType) list) (id: id) :primitiveType =
  if(List.mem id ["from"; "to"; "rel"])
  then(gen_new_rec([]))
  else(
  match elist with
  [] -> raise (failwith (id ^ "  not defined @ 133"))
  |(field, typ) :: tail -> if(field = id) then(typ) else(get_field_type tail id))

let rec check_field (fields: ((id * primitiveType) * (id * primitiveType))) : unit =
  match fields with
  |(id1, t1), (id2, t2) -> if(id1 = id2) then(check_compatible_types (t1,t2)) else(raise(failwith("mismatched fields " ^ id1 ^ " & " ^ id2)))

and check_compatible_types (t: primitiveType * primitiveType) : unit =
  match t with 
   |T(_), a | TVoid, a | a, TVoid | a, T(_) -> ()
   |TList(_), TList(T(_)) | TList(T(_)), TList(_) -> ()
   |TRec(a, b), TRec(c, d) -> ignore(let fieldslists = List.combine b d in List.map (fun a -> check_field a) fieldslists); ()
   |TEdge(_, b, c), TEdge(_,e,f) -> ignore(check_compatible_types (b,e)); check_compatible_types (c,f)
   |TGraph(a, b, c), TGraph(_, e, f) -> ignore(check_compatible_types (b,e)); check_compatible_types (c,f)
   |a, b -> if(a = b) then () else raise(failwith("type mismatch: " ^ (string_of_type a) ^ ","  ^(string_of_type b) ^ "@118"))

(*Ensures all members of a list share the same type.*)
let rec check_type_consistency (tl: primitiveType list) : unit =
  match tl with 
  |x :: y :: t -> 
  ignore(check_compatible_types (x,y));
  check_type_consistency (y :: t)
  |[] | _ -> ()

(* A function is a list of statements. Each statement's expressions are inferred here. 
The result is annotated and passed into the sast. *)
let rec infer_stmt_list (allenv: allenv) (e: stmt list) : (allenv * astmt list) =
   let rec helper allenv astmts stmts  : (allenv * astmt list) = 
    match stmts with
      [] -> (allenv, List.rev astmts)
      |fst :: snd :: tail -> 
      let allenv, ae = type_stmt allenv fst in 
      let allenv, ae2 = type_stmt allenv snd in       
      (match ae with 
      |AReturn(ae, _) -> raise(failwith("error: unreachable statment " ^ string_of_astmt ae2));
      |_ -> (helper allenv (ae2 :: ae :: astmts) tail))
      |x :: tail -> let allenv, ae = type_stmt allenv x in helper allenv (ae :: astmts) tail
  in helper allenv [] e 

and type_stmt (allenv: allenv) (e: stmt) : allenv * astmt  = 
  let allenv, astmt = infer_stmt allenv e in 
  let _, genv,recs,funcs = allenv in 
  let env,_,recs,_ = update_map allenv astmt in
  ((env, genv, recs,funcs), astmt)

and infer_stmt (allenv: allenv) (e: stmt): (allenv * astmt) =
(*   ignore(print_string ("\ninferring " ^ (string_of_stmt e)));   *) 
  let env, genv, recs, funcs = allenv in 
  let allenv, inferred_astmt = 
  match e with
  | Asn(e1, e2, switch) -> 
    let ae2 = infer_expr allenv e2 in 
    let typ = type_of ae2 in
    let ae1, env = 
          match e1 with
          |Id(a) -> 
          let id = map_id a in 
          let env =  (* if a variable is first encountered here, add it to env*)
          if NameMap.mem (id) env
          then (let otype = type_of (infer_expr allenv e1) in
                ignore(check_compatible_types (otype, typ)); env)
          else (NameMap.add id (gen_new_type()) env) in
          AId(a, typ), env   
          |Item(a,_)|Dot(Id(a),_) -> 
          let id = map_id a in
          if(NameMap.mem id env) 
          then(infer_expr (env, genv, recs, funcs) e1, env) 
          else(raise(failwith(id ^ " not defined.")))
          |x -> raise(failwith(string_of_expr x ^ " is not a valid lval"))
    in
    (allenv, AAsn(ae1, ae2, switch, typ))
  | Return(expr) ->
    let aexpr = infer_expr allenv expr in 
    let allenv = env, genv, recs, funcs in 
    (allenv, AReturn(aexpr, type_of aexpr))
  | Expr(expr) -> 
    let aexpr = infer_expr allenv expr in 
    let allenv = env, genv, recs, funcs in 
    (allenv, AExpr(aexpr))
  | If(expr, s1, s2) -> 
    (* Statement blocks only modify the environment in the block *)
    let conditional = infer_expr allenv expr in 
    (check_bool conditional);
    let ((_,genv,_,funcs), as1) = infer_stmt_list allenv s1 in                   
    let ((_,genv,_,funcs), as2) = infer_stmt_list (env,genv,recs,funcs) s2 in
    let allenv = env, genv, recs, funcs in 
    (allenv, AIf(conditional, as1, as2))
  | While(e1, s1s) ->
    let ae1 = infer_expr allenv e1 in ignore(check_bool ae1); 
    let ((_,genv,_,_funcs), as1s) = infer_stmt_list allenv s1s in 
    let allenv = env, genv, recs, funcs in 
    (allenv, AWhile(ae1, as1s))
  | For(s1, e1, s2, stmts) -> 
    (check_asn s1);
    (check_asn s2);
    let outerenv = allenv in
    let (allenv, as1) = type_stmt allenv s1 in 
    let ae1 = infer_expr allenv e1 
    in (check_bool ae1);
    let (allenv, as2) = (type_stmt allenv s2) in
    let _, astmts = infer_stmt_list allenv stmts in 
     (outerenv, AFor(as1, ae1, as2, astmts))
  | Forin(e1, e2, stmts) -> 
    let outerenv = allenv in
    let env, genv, recs, funcs = allenv in 
    let id = (get_id e1) in 
    let ae2 = infer_expr allenv e2 in   
    let it = get_subtype (type_of ae2) in 
    let env = NameMap.add (map_id id) it env in 
    let allenv = env, genv, recs, funcs in 
    let aid = infer_expr allenv e1 in
    let _, astmts = infer_stmt_list allenv stmts in  (*change type_stmt to update the map*)
     (outerenv, AForin(aid, ae2, astmts))
    in let env, genv, recs, funcs = allenv in
    let funcs = update_funcs inferred_astmt funcs genv 
  in ((env,genv,recs,funcs), inferred_astmt)

(*Called from annotate_stmt, infers expressions inside statements.*)
and infer_expr (allenv: allenv) (e: expr): (aexpr)  =
  let annotated_expr = annotate_expr allenv e in
  let constraints = collect_expr annotated_expr in 
  let subs = unify constraints in
  let ret = apply_expr subs annotated_expr in ret

(*Step 1 of HM: annotate expressions with what we know of their types.*)
and annotate_expr (allenv: allenv) (e: expr) : aexpr =
let env, genv, recs, funcs = allenv in
  match e with
  | IntLit(n) -> AIntLit(n, TInt)
  | BoolLit(b) -> ABoolLit(b, TBool)
  | StrLit(s) -> AStrLit(s,TString)
  | FloatLit(f) -> AFloatLit(f, TFloat)
  | CharLit(c) -> ACharLit(c, TChar)
  | Noexpr -> ANoexpr(gen_new_type())
  | Id(x) -> 
    let typ = find_in_map x env in 
    (match typ with
     |t ->  AId(x, t))
  | Item(s, e) -> 
    let et1 = annotate_expr allenv e in 
    let typ = find_in_map s env in
    (match typ with
     |TVoid -> raise (failwith (s ^ " not defined @ 115."))
     |TList(t) -> AItem(s, et1, t)
     |T(a) -> AItem(s, et1, gen_new_type())
     |t -> raise (failwith (string_of_type (t) ^ " not a list.")))
  | Binop(e1, op, e2) ->
    let et1 = annotate_expr allenv e1
    and et2 = annotate_expr allenv e2
    and new_type = gen_new_type () in
    ABinop(et1, op, et2, new_type)
  | Unop(uop, e1) ->
    let et1 = annotate_expr allenv e1 and t = gen_new_type() in 
    AUnop(uop, et1, t)
  | Dot(e1, entry) -> 
    let ae1 = annotate_expr allenv e1 in 
    let et1 = type_of ae1 in
    let sae1 = string_of_aexpr ae1 in
    let typ =   
         (match et1 with
          |TRec(str, elist) -> 
          get_field_type elist entry
          |TGraph(_,n,e) ->
          (match entry with
          |"edges" -> TList(e)
          |"nodes" -> TList(n)
          |_ -> raise(failwith(entry ^ " not a field."))
          )
          |TEdge(a,n,e) -> 
          (match entry with 
          |"from" |"to" -> n 
          |"dir" -> TBool
          |"rel" -> e
          | _ -> raise(failwith(entry ^ " not a field."))
          )
          |T(x) -> T(x)
          |x -> raise(failwith (sae1 ^ " not a record.")))    
    in ADot(ae1, entry, typ)
  | List(e) -> 
    let ael = List.map (fun a -> annotate_expr allenv a) e in 
    let len = List.length ael in 
    if (len = 0)
    then (AList(ael, TList(gen_new_type())))
    else (ignore(check_type_consistency (List.map (fun a -> type_of a) ael));
        let tl = List.nth ael (len-1) in 
        let t = (type_of (tl)) in 
       AList(ael, TList(t)))
  | Call(id, elist) ->  
    let aelist = List.map (fun a -> infer_expr allenv a) elist in
    let callingfunc = Stack.top callstack in
    Stack.push id callstack;     
    let (oldtype, aformals, stmts) =
      if (NameMap.mem id genv)
      then (NameMap.find id genv)
      else (raise (failwith "function not defined @ 147")) in
    if(id=callingfunc) 
    then(ACall(id, aelist, [], id, oldtype)) (*no infinite loops. Give the correct statements here?? *)
    else(
    ignore(let len = List.length aformals in
    if (List.length aelist != len)
    then(raise(failwith("error: " ^ id ^ " takes " ^ (string_of_int len) ^ " formal/s")))
    else());      
    (* Here we reinfer the function for the call by mapping the formals to the actuals. *)
    let env = update_types_formals (List.combine aformals aelist) env id in
    let allenv = env, genv, recs, funcs in
    ignore(check_formals aformals allenv);
    let (_, astmts) = (infer_stmt_list allenv stmts) in
    let t = get_return_type astmts in 
    ignore(Stack.pop callstack); 
    let in_id = get_func_name id in (* the id for this call of the function. *)
    ACall(id, aelist, astmts, in_id, t))
| Record(pairlist) -> 
    let rec helper(l: (string * expr) list) =
    match l with
    [] -> []
    |(id, expr) :: tl ->
    (id, (annotate_expr allenv expr)) :: helper tl 
    in let apairlist = helper (List.sort comp pairlist) in
    ignore(if(has_dups pairlist) then(raise(failwith("error: duplicate record entry"))) else());
    let typ = get_rec recs apairlist in 
    ARecord(apairlist, typ)
 | Edge(e1, op, e2, e3) -> 
   let ae1 = annotate_expr allenv e1 and
       ae2 = annotate_expr allenv e2 and
       ae3 = annotate_expr allenv e3 in 
      AEdge(ae1, op, ae2, ae3, TEdge(gen_new_type(), type_of ae1, type_of ae3))
 | Graph(elist, tedge) ->
   let aelist = List.map (fun a -> infer_expr allenv a) elist in
   let edgelist, nodelist = split_types aelist in
   ignore(check_type_consistency (edgelist));
   ignore(check_type_consistency (nodelist)); 

   let atemplate = infer_expr allenv tedge in 
   let etype = type_of atemplate in 
   let ntype = 
   if(List.length nodelist = 0) then(gen_new_rec([])) else(List.hd nodelist) in 
   let edgetype = if(List.length edgelist = 0) 
                  then(TEdge(gen_new_type(), gen_new_type(), gen_new_type())) 
                  else(List.hd edgelist) in 
   let gtype = match edgetype with 
   |TEdge(name, nt, et) -> TEdge(name, ntype, etype) in 
   let aelist = enforce_node_consistency aelist (gtype) in
   AGraph(aelist, atemplate, TGraph(gen_new_type(), ntype, gtype))
  (*a. check the list for consistency between nodes and edges. (which could be noexprs or lists themselves, or type of e.)
    b. Edge template type imposes constraints on nodes.
    c. what if there are no nodes? Graph should be a trec of any, and should be overwritable when the first node comes in.
   Remember, edges have nodes in them. *)

(*Ensures actuals and their corresponding formals have compatible types. *)
and check_formals (aformals: (id * primitiveType) list) (allenv: allenv)  : unit =
(*   ignore(print_string("checking formals\n"));  *)
  let env, _,_,_ = allenv in 
  List.iter(fun (id, t) -> let nt = find_in_map id env in ignore(check_compatible_types(nt,t))) aformals

(*Step 2 of HM: Collect constraints*)
and collect_expr (ae: aexpr) : (primitiveType * primitiveType) list =
  match ae with
  | AIntLit(_,_) | ABoolLit(_,_) | AStrLit(_,_) | AFloatLit(_,_) 
  | ACharLit(_,_) | ARecord(_,_) | AGraph(_,_,_) | AId(_,_) -> []
  | AUnop(uop, ae1, t) ->
    let et1 = type_of ae1 in 
    let opc = match uop with
    | Not -> [(et1, TBool); (t, TBool)]
    | Neg -> [(et1, TInt); (t, TInt)]
  in (collect_expr ae1) @ opc
  | ABinop(ae1, op, ae2, t) ->
    let et1 = type_of ae1 and et2 = type_of ae2 in
    let opc = match op with
      | Add | Mult | Sub | Div -> [(et1, TInt); (et2, TInt); (t, TInt)]
      (* we return et1, et2 since these are generic operators *)
      | Greater | Less | Equal | Geq | Leq | Neq -> check_compatible_types(et1, et2); [(t, TBool)]
      | And | Or -> [(et1, TBool); (et2, TBool); (t, TBool)]
      | Fadd | Fsub | Fmult | Fdiv -> [(et1, TFloat); (et2, TFloat); (t, TFloat)]
      | Ladd -> [(et1, TList(et2)); (t, TList(et2))]
      | In -> 
      (match et2 with |TList(x) ->     
              [(et1, x); 
              (et2, TList(gen_new_type())); 
              (t, TBool)]
                      | _ -> raise(failwith("Error @330")))
      | Gadd ->  [(t, et1)]
      | Eadd -> 
       (match et1, et2 with |TGraph(name, n, e), TEdge(_,_,_) -> [(t, et1); (et2, e)]
                            | _ -> [(t, et1)])      
      | _ -> raise(failwith("error"))
     in
    (collect_expr ae1) @ (collect_expr ae2) @ opc 
  (*opc appended at the rightmost since we apply substitutions right to left *)
  | AEdge(ae1, op, ae2, ae3, t) ->
    let et1 = type_of ae1 and et2 = type_of ae2 and et3 = type_of ae3 in 
    let opc = match op with
          | To | From | Dash ->
          (match et1, et2 with
          |TRec(_,_), TRec(_,_) -> ignore(check_compatible_types (et1,et2)); []
          | _ -> raise(failwith("error: " ^ string_of_aexpr ae1 ^ " and " ^ string_of_aexpr ae2 ^ " must be nodes."))
          )
          | _ -> raise(failwith((string_of_op op) ^ " not an edge operator."))
     in 
     ignore(match et3 with
        | TRec(_,_) | T(_) -> ()
        | _ -> raise(failwith("error: " ^ string_of_aexpr ae3 ^ " not a record.")));
     (collect_expr ae1) @ (collect_expr ae2) @ opc @ (collect_expr ae3)     
  | ADot(ae1, _, _) -> []
  | AItem(s, ae1, t) -> collect_expr ae1 @ [((type_of ae1), TInt)]
  | ACall(_, _, _, _, _) 
  | ANoexpr(_) -> []
  | AList(ael, t) -> []

(*Step 3 of HM: unify constraints*)
and unify (constraints: (primitiveType * primitiveType) list) : substitutions =
  match constraints with
  | [] -> []
  | (x, y) :: xs ->
    (* generate substitutions of the rest of the list *)
    let t2 = unify xs in
    (* resolve the LHS and RHS of the constraints from the previous substitutions *)
    let t1 = unify_one (apply t2 x) (apply t2 y) in
    (*     ignore(print_string ("after unify one\n")); *)
    t1 @ t2
and unify_one (t1: primitiveType) (t2: primitiveType) : substitutions =
  match t1, t2 with
  | TInt, TInt | TBool, TBool | TString, TString | TFloat, TFloat | TVoid, TVoid -> []
  | T(x), z | z, T(x) -> [(x, z)]
  | TList(x), TList(y) -> unify_one x y
  | TGraph(name1, a, b), TGraph(name2, c, d) -> unify_one a c @ unify_one b d 
  | TEdge(name1, n1, e1), TEdge(name2, n2, e2) ->
    unify_one name1 (TEdge(name2, n2, e2))
  | TRec(a, b), TRec(c, d) -> 
    ignore(let fieldslists = List.combine b d in List.map (fun x -> check_field x) fieldslists);
    unify_one a c
  | _ -> raise (failwith "mismatched types@502")
and substitute (u: primitiveType) (x: id) (t: primitiveType) : primitiveType =
  match t with
  | TInt | TBool | TString | TFloat | TList(_) | TChar| TVoid-> t 
  | T(c) | TRec(T(c),_)  | TEdge(T(c),_,_) | TGraph(T(c),_,_) -> if c = x then u else t 
and apply (subs: substitutions) (t: primitiveType) : primitiveType =
  List.fold_right (fun (x, u) t -> substitute u x t) subs t

(*Step 4: Final application of substitutions*) 
and apply_expr (subs: substitutions) (ae: aexpr): aexpr =
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
  | AItem(s, e1, t) -> let ae1 = apply_expr subs e1 in (* ignore(check_int ae1); *) AItem(s, ae1, apply subs t)
  | ACall(name, e, astmts, id, t) -> ACall(name, e, astmts, id, apply subs t)
  | ADot(id, entry, t) -> ADot(apply_expr subs id, entry, apply subs t) (*Am I handling this right?*)
  | AEdge(e1, op, e2, e3, t) -> AEdge(apply_expr subs e1, op, apply_expr subs e2, apply_expr subs e3, apply subs t)
  | ANoexpr(t) -> ANoexpr(t) (*is this okay?*)
and apply_expr_list (subs: substitutions) (ae: aexpr list)  : aexpr list =
  let rec helper (ae: aexpr list) (res: aexpr list) = 
    match ae with
      [] -> List.rev res
    |h :: t ->  helper t (apply_expr subs h :: res) 
  in helper ae []

(*Helper function for update map*)
and assign (ae: aexpr) (ae2: aexpr) (env: environment) : environment =
  let t = type_of ae2 in 
  let env = 
  match ae with
  |AId(str, _) -> NameMap.add (map_id str) t env
  |ADot(AId(_, TRec(T(recname), _)), str, _) -> NameMap.add (map_id (map_id_rec recname str)) t env
  |AItem(str, _, _) -> NameMap.add (map_id str) (TList(t)) env
  |_ -> raise(failwith("error: " ^ string_of_aexpr ae ^ " not a valid lvalue@534."))
in env

(*Updates environment*)
and update_map (allenv: allenv) (a: astmt) : allenv = 
  let env, genv, recs, funcs = allenv in
  match a with
  |AAsn(ae1, ae2, _,_) ->
    let env, recs = (update_map_recs (type_of ae2) (env, recs)) in
    let env = assign ae1 ae2 env in 
   env, genv, recs, funcs
  |_ -> allenv 

(* get the template we generate from call. *)
and update_funcs (a: astmt) (funcs: funcs) (genv: genvironment) : funcs = 
  match a with
  |AReturn(ae, _) 
  |AExpr(ae) 
  |AWhile(ae, _) 
  |AAsn(_,ae, _,_) -> apply_update ae funcs genv
  |AIf(ae, s1, s2) -> let funcs = apply_update ae funcs genv in 
                      let funcs = List.fold_left (fun a b -> update_funcs b a genv) funcs s1 in 
                      List.fold_left (fun a b -> update_funcs b a genv) funcs s2
  |AFor(s1, ae, s2, s3s) -> 
                      let funcs = update_funcs s1 funcs genv in 
                      let funcs = apply_update ae funcs genv in 
                      let funcs = update_funcs s2 funcs genv in
                      List.fold_left (fun a b -> update_funcs b a genv) funcs s3s
  |AForin(_,_,s1s) -> List.fold_left (fun a b -> update_funcs b a genv) funcs s1s
and apply_update (call: aexpr) (funcs: funcs) (genv: genvironment ) : funcs = 
  match call with 
  | AIntLit(_,_)| ABoolLit(_,_) | AFloatLit(_,_) | AStrLit(_,_) | ACharLit(_,_) | AId(_,_) -> funcs
  | AItem(_, e, _) | AUnop(_, e, _) | ADot(e,_,_) -> apply_update e funcs genv

  | ABinop(e1, _, e2, _) -> let funcs = apply_update e1 funcs genv in apply_update e2 funcs genv
  | AEdge(e1, _, e2, e3, _) -> let funcs = apply_update e1 funcs genv in 
                               let funcs = apply_update e2 funcs genv in 
                               apply_update e3 funcs genv
  | AList(elist, _)  -> List.fold_left (fun a b -> apply_update b a genv) funcs elist 
  | AGraph(elist, e1, _) -> let funcs = List.fold_left (fun a b -> apply_update b a genv) funcs elist in 
                            apply_update e1 funcs genv
  | ANoexpr(_) -> funcs
  |ACall(name, aelist, astmts, id, t) -> 
   let funcs = List.fold_left (fun a b -> apply_update b a genv) funcs aelist in 
   let (_, aformals, _) =
      if (NameMap.mem (name) genv)
      then (NameMap.find (name) genv)
      else (raise (failwith "function not defined @ 601")) in 
  let flist = List.combine aformals aelist in 
  let aformals = List.map format_formal flist in  
  ((AFbody(AFdecl(id, aformals, t), astmts)) :: funcs)
  |_ -> funcs

 (*Used when an expression itself changes the environment, i.e, in records or calls 
 that are secretly records. *)
 and update_map_recs (t: primitiveType) (env, recs: environment * recs) : environment * recs = 
    (match t with
    |TRec(T(tname), elist) -> 
      let rec helper l env = 
      (match l with
      |[] -> env
      |(field, fieldtype) :: tail -> 
      let env = NameMap.add (map_id (map_id_rec tname field)) fieldtype env in helper tail env
      )
      in 
      let recs = if(NameMap.mem tname env)
      then(recs)
      else(t :: recs) in
      let env = helper elist env in
    (env, recs)
    |_ -> env, recs)

(*Returns the type for functions.*)
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
      |AForin(_, _, y) ->
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
      [] -> TVoid    | [t] -> t 
    | x :: y :: tail -> 
     raise (failwith "Error: multiple returns.");
  in (find_type returns)

(* Infer formals from function statements. *)
and infer_formals (formals: string list)  (env: environment):  (string * primitiveType) list =
  let rec helper f env aformals = 
  match f with
  |[] -> List.rev aformals
  | h :: tail -> 
    let t = find_in_map h env in  
    helper tail env ((h, t) :: aformals) in
  helper formals env []

(*The calling method for this file. Infers all types for a func (statements, formals), and
outputs an annotated func. *)
and infer_func (allenv: allenv) (f: func) :  (afunc list * genvironment)  =
(*   ignore(print_string("inferring new func\n")); *)
  let env, genv, recs, funcs = allenv in
  match f with
  |Fbody(decl, stmts) -> 
    ignore(match decl with Fdecl(fname, _)-> Stack.push fname callstack); (*set scope*)
    let ((_,genv,_,funcs), istmts) = infer_stmt_list allenv stmts (*infer the function statments*)
    in let ret_type = get_return_type istmts                       
    in match decl with
    |Fdecl(fname, formals) ->           (*add function to NameMap*) 
      if NameMap.mem fname genv
      then(
        let aformals = infer_formals formals env in   
        let genv = NameMap.add fname (ret_type, aformals, stmts) genv in 
        let allenv = env, genv, recs, funcs in
        let ((env, genv, recs, funcs), astmts) = infer_stmt_list allenv stmts in  
        (ignore(Stack.pop callstack));
        let toss =  List.fold_left (fun hasany h -> match h with |(_,T(_)) -> true |_ -> hasany) false aformals in 
        let funcs = 
        match ret_type with 
        T(_) -> funcs   
        |_ -> if(toss) then(funcs) else(AFbody(AFdecl(fname, aformals, ret_type), astmts) :: funcs)
      in funcs, genv) 
      else raise (failwith "function not defined @ 412")