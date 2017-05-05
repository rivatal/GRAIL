(*https://github.com/prakhar1989/type-inference/blob/master/infer.ml*)
(*Assoc Arrays! And fancy subset?*)
open Ast
open Astutils   

module NameMap = Map.Make(String)
  type environment = primitiveType NameMap.t

type genvironment = (primitiveType * (string * primitiveType) list * stmt list) NameMap.t
let callstack = Stack.create()
type allenv = environment * genvironment

(* Unknown type,  resolved type. eg.[(T, TInt); (U, TBool)] *)
type substitutions = (id * primitiveType) list


let map_id (id: string) : string =
  let fname = Stack.top callstack in
  (map_id_with fname id)

(* generates a new unknown type placeholder.
 * returns T(string) of the generated alphabet *)
let type_variable = ref 1
let gen_new_type () =
  let c1 = !type_variable in
  incr type_variable; 
  T(string_of_int c1)

let gen_new_void () : primitiveType =
  TVoid (*just chr escaped, no T in the TVoid*)

let type_of (ae: aexpr): primitiveType =  
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

(*Comparator used in annotating records.*)
let comp (x: id * expr) (y: id * expr) : int = 
  match x, y with
  |(a,_), (b,_) -> if(a = b) then(0) else(if(a < b) then(-1) else(1))

(*Helper function for annotating records (check for duplicate fields)*)
let rec has_dups l = 
  match l with
  |(a,_) :: (b,c) :: tail -> if(a = b) then(true) else(has_dups((b,c)::tail))
  |[]| _ -> false


(*Store variables with record names*)
let map_id_rec (rname: string) (id: string) : string =
(*   ignore(print_string ("getting name: " ^ rname ^ ";" ^ id ^ "\n"));  *)
  rname ^ ";" ^ id

(*finds the variable in the map*)
let rec find_in_map(id: string) (env: environment): primitiveType =
  let mapped = map_id id in
   if (NameMap.mem mapped env)  
   then (NameMap.find mapped env)
   else (raise(failwith(mapped ^ " not found@79")))

let rec get_type_list (aelist: aexpr list) : primitiveType list = 
  List.map type_of aelist

let rec split_list (aelist: aexpr list) : (primitiveType list * primitiveType list) = 
  let rec helper l edgelist nodelist : (primitiveType list * primitiveType list) =
  (match l with 
  |[] -> edgelist, nodelist
  |et1 :: t ->
  (match et1 with
  |TRec(_,_) -> helper t edgelist (et1 :: nodelist)
  |TEdge(_) -> helper t (et1 :: edgelist) nodelist
  |T(_) | TVoid -> helper t edgelist nodelist
  |TList(typ) -> helper (typ :: t) edgelist nodelist
  |x -> raise(failwith(string_of_type x ^ " not a graph type."));
  )) 
  in (helper (get_type_list aelist) [] [])

(*A checking function for something like the first field of a for*)
let check_asn (a: stmt) : unit =
  (*   print_string "Checking assign\n";*)  
  match a with
    Asn(_,_,_) -> ()
  |_ -> raise(failwith ((string_of_stmt a) ^ " not an assignment statement."))

(*Ensures all members of a list share the same type.*)
let rec check_type_consistency (tl: primitiveType list) : unit =
  match tl with 
  |x :: y :: t -> 
  (match x, y with
     | a, T(_) | a, TVoid -> check_type_consistency (x :: t)
     | T(_), a | TVoid, a-> check_type_consistency (y :: t)
     | TEdge(TRec(_, _)), TEdge(T(_)) -> check_type_consistency (x :: t)
     | TEdge(T(_)), TEdge(TRec(_, _)) -> check_type_consistency (y :: t)
     | a, b -> 
     if(a = b) 
     then(check_type_consistency (y :: t))
     else(raise (failwith ("List: mismatched types " ^ (string_of_type x) ^ ", " ^ (string_of_type y) ^ "\n")))
  )
  |[] | _ -> ()

let rec check_list_consistency (e: aexpr list) : unit =
  check_type_consistency (get_type_list e)

(*Searches a list of record fields for a particular id and gets its type*)
let rec get_field_type (elist: (id * primitiveType) list) (id: id) =
  match elist with
  [] -> raise (failwith (id ^ "  not defined @ 133"))
  |(field, typ) :: tail -> if(field = id) then(typ) else(get_field_type tail id)


(* A function is a list of statements. Each statement's expressions are inferred here.
The result is annotated and passed into the sast. *)
let rec infer_stmt (allenv: allenv) (e: stmt): (allenv * astmt) =
(*   ignore(print_string (" inferring " ^ (string_of_stmt e) ^ "\n"));   *)
  let env, genv = allenv in 
  match e with
  | Asn(e1, e2, switch) -> 
    let ae2 = infer_expr allenv e2 in 
    let typ = type_of ae2 in
    let ae1, env = 
          match e1 with (*Maybe make into an id map thing?*)
          |Id(a) -> 
          let id = map_id a in 
          let env = 
          if NameMap.mem (id) env
          then (
            let otype = type_of (infer_expr allenv e1) in
            ignore(check_asn_type otype typ); env
          )
          else (NameMap.add id (gen_new_type()) env) in
          AId(a, typ), env 
          |Item(a,_)|Dot(Id(a),_) -> 
          let id = map_id a in
          if(NameMap.mem id env) 
          then(infer_expr (env, genv) e1, env) 
          else(raise(failwith(id ^ " not defined.")))
          |x -> raise(failwith(string_of_expr x ^ " is not a valid lval"))
    in
    (* ignore(print_string("146- " ^ string_of_type (type_of ae2)   )); *)
    (allenv, AAsn(ae1, ae2, switch, typ))
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
  | While(e1, stmts) ->
    let ae1 = infer_expr allenv e1 in ignore(check_bool ae1); 
    let (_, astmts) = infer_stmt_list allenv stmts in 
    (allenv, AWhile(ae1, astmts))
  | For(s1, e1, s2, stmts) -> 
    let outerenv = allenv in
    (check_asn s1);
    (check_asn s2);
    let (allenv, as1) = type_stmt allenv s1 in 
    let ae1 = infer_expr allenv e1 
    in (check_bool ae1);
    let (allenv, as2) = (type_stmt allenv s2) in
    let _, astmts = infer_stmt_list allenv stmts in  (*change type_stmt to update the map*)
    let allenv = outerenv in
     (allenv, AFor(as1, ae1, as2, astmts))
  | Forin(e1, e2, stmts) -> 
    let outerenv = allenv in
    let env, genv = allenv in 
    let id = (get_id e1) in 
    let ae2 = infer_expr allenv e2 in 
    
    let it = get_subtype (type_of ae2) in 
    let env = NameMap.add (map_id id) it env in 
    let allenv = env, genv in 
    let aid = infer_expr allenv e1 in
    let _, astmts = infer_stmt_list allenv stmts in  (*change type_stmt to update the map*)
     (outerenv, AForin(aid, ae2 , astmts))

and type_stmt (allenv: allenv) (e: stmt) : allenv * astmt  = 
  let allenv, astmt = infer_stmt allenv e in 
  let _, genv = allenv in 
  let env = update_map allenv astmt in
  ((env, genv), astmt)

and infer_stmt_list (allenv: allenv) (e: stmt list) : (allenv * astmt list) =
   let rec helper allenv astmts stmts  : (allenv * astmt list) = 
    match stmts with
      [] -> (allenv, List.rev astmts)
     |head :: tail -> 
      let allenv, ae = type_stmt allenv head in 
      (helper allenv (ae :: astmts) tail)
  in helper allenv [] e 

(*Step 1 of HM: annotate expressions with what can be gathered of their types.*)
and annotate_expr (allenv: allenv) (e: expr) (* (env: environment) *) : aexpr =
(*   ignore(print_string ("annotating " ^ (string_of_expr e) ^ "\n"));     *)  
let env, genv = allenv in
  match e with
  | IntLit(n) -> AIntLit(n, TInt)
  | BoolLit(b) -> ABoolLit(b, TBool)
  | StrLit(s) -> AStrLit(s,TString)
  | FloatLit(f) -> AFloatLit(f, TFloat)
  | CharLit(c) -> ACharLit(c, TChar)
  | Id(x) -> 
    let typ = find_in_map x env in 
    (match typ with
     |t ->  AId(x, t))
  | Item(s, e) -> 
    let et1 = annotate_expr allenv e in 
    let typ = find_in_map s env in
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
(*     ignore(print_string "annotating dot\n"); *)
    let ae1 = annotate_expr allenv e1 in 
    let et1 = type_of ae1 in
    let sae1 = string_of_aexpr ae1 in
    let typ = 
         (match et1 with
          |TRec(str, elist) -> 
          get_field_type elist entry
          |T(x) -> T(x)
          |x -> raise(failwith (sae1 ^ " not a record.")))    
    in ADot(ae1, entry, typ)
  | List(e) -> 
    let ael = annotate_expr_list allenv e in 
    (match ael with
       [] -> AList(ael, TList(gen_new_type()))
     | head :: _ -> let t = type_of head
       in 
(*        ignore(check_list_exprs ael); *)
       ignore(check_list_consistency ael);
       AList(ael, TList(t)))
  | Call(id, elist) ->    (*Function calls derive their type from the function declaration*)
    let aelist = List.map (fun a -> infer_expr allenv a) elist in
    Stack.push id callstack;     
    let (oldtype, aformals, stmts) =
      if (NameMap.mem id genv)
      then (NameMap.find id genv)
      else (raise (failwith "function not defined @ 147")) in
(*     ignore(print_string("assigning formals")); *)    
    let env = assign_formals (List.combine aformals aelist) env id in
    let allenv = env, genv in
    (* ignore(print_string("check formals"));*)
    ignore(check_formals aformals allenv);
    let (_, astmts) = (infer_stmt_list allenv stmts) in
    let t = get_return_type astmts in 
    ignore(Stack.pop callstack);
    ACall(id, aelist, astmts, t) 
| Record(pairlist) -> 
    let rec helper(l: (string * expr) list) =
    match l with
    [] -> []
    |(id, expr) :: tl ->
    (id, (annotate_expr allenv expr)) :: helper tl 
    in let apairlist = helper (List.sort comp pairlist) in
    ignore(if(has_dups pairlist) then(raise(failwith("error: duplicate record entry"))) else());
    (*ignore(print_string ("record is size " ^ string_of_int (List.length apairlist) ^ "\n")); *) 
    ARecord(apairlist, gen_new_rec(apairlist))
   (* type records = (primitiveType * ((id * primitiveType) list)) list *)
 | Graph(elist, tedge) ->
   let atedge = annotate_expr allenv (Edge(Noexpr, Dash, Noexpr, tedge)) in
   let aelist = annotate_expr_list allenv (elist) in
   
   let temptype = type_of atedge in 
   let edgelist, nodelist = split_list aelist in
   ignore(check_type_consistency (temptype :: edgelist));
   ignore(check_type_consistency (nodelist)); 
   let gtype = if(List.length nodelist = 0) then(gen_new_type()) else(List.hd nodelist) in
   
   AGraph(aelist, atedge, TGraph(gtype, temptype))
  (*a. check the list for consistency between nodes and edges. (which could be noexprs or lists themselves, or type of e.)
    b. type of e imposes a constraint on ^ and on the graph type. 
    c-- what if there are no nodes? Graph should be a trec of any, and should be overwritable when the first node comes in.
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


(*Generate unique record type based on fields*)
and gen_new_rec (fieldslist : (id * aexpr) list) : primitiveType =
  let rec helper l : (id * primitiveType) list= 
  (match l with
  [] -> []
  | (id, aexpr) :: tl -> 
  (id, type_of aexpr) :: helper tl
  )
  in let fields = helper (List.rev fieldslist)
  in let c1 = !type_variable in
  incr type_variable; 
  TRec(string_of_int c1, fields)

(*ensure thing you're assigning to has that type. (No my_bool = 3; )*)
and check_asn_type (lval: primitiveType) (asn: primitiveType) : unit  =
  match lval with
  |TVoid | T(_) -> ()
  | x -> if(x = asn) then(()) else (
    match lval, asn with
    |TEdge(a), TEdge(b)-> check_asn_type a b
    |TGraph(n1, e1), TGraph(n2, e2) -> ignore(check_asn_type n1 n2); (check_asn_type e1 e2)
    |TList(a), b -> ignore(check_asn_type a b);
    |_ -> raise(failwith("error: " ^ string_of_type asn ^ " was defined as " ^ string_of_type x))
   )

(*Ensures an expression is a conditional (e.g. for predicate statements)*)
and check_bool (e: aexpr) : unit =
  (*   print_string "Checking bool"; *)
  if(type_of e != TBool)
  then(raise(failwith ((string_of_aexpr e) ^ " not a boolean.")))
  else ()

and get_id (e: expr) : string =
  match e with
  |Id(str) -> str 
  |_ -> raise(failwith(string_of_expr e ^ " is not an id."))

and get_subtype (t: primitiveType) : primitiveType =
  match t with
  |(* TGraph(_,_) |  *)TList(st) -> st
  | T(_) -> t 
  |x -> raise(failwith("error: " ^ string_of_type x ^ " not iterable."))

(* and check_list_exprs (e: aexpr list) : unit =
  match e with 
    [] -> ()
  |h :: t -> 
    match h with
    | AIntLit(_,_) | ABoolLit(_,_) | AStrLit(_,_) | AFloatLit(_,_) | ACharLit(_,_) | AId(_,_) | AList(_,_) -> check_list_exprs t
    | y -> raise(failwith ((string_of_aexpr y) ^ " does not belong in a list."))
 *)
(*Generates assignment statements for actual expressions to be inferred and bound to their formals*)
and assign_formals (stufflist: ((id * primitiveType) * aexpr) list) (env: environment) (id: string): environment =
  let rec helper env l   = 
    match l with
      [] -> env
    |h :: t ->     (*_ is the old "bad" type*)
      match h with
        ((x, _), e) -> 
        let env = NameMap.add (map_id x) (type_of e) env in 
        helper env t (*Really should make x's type the original formal types...?*)
  in helper env stufflist

(*Ensures actuals and their corresponding formals have the same type. 
Required for builtin functions like print.*)
and check_formals (aformals: (id * primitiveType) list) (allenv: allenv)  : unit =
(*   ignore(print_string("checking formals\n"));  *)
  let env, _ = allenv in 
  let rec helper af env = 
  match af with 
  [] -> ()
  |(id, typ) :: tail ->
   let newtype = NameMap.find (map_id id) env in
  (match newtype with
   |T(_) | TVoid -> ()
   | nt -> if(nt = typ) 
    then(helper tail env) 
    else(match typ with
        |T(_) | TVoid -> ()
        | _ -> raise(failwith("Error: " ^ string_of_type nt ^ " not a valid for " ^ string_of_type typ ^ " in function."))))
in helper aformals env

(*Step 2 of HM: Collect constraints*)
and collect_expr (ae: aexpr) : (primitiveType * primitiveType) list =
(*     ignore(print_string "collecting\n"); *)
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
      | Ladd -> [(et1, TList(et2)); (t, TList(et2))]
      | In -> 
      (match et2 with |TList(x) ->     
              [(et1, x); 
              (et2, TList(gen_new_type())); 
              (t, TBool)]
                      | _ -> raise(failwith("Error @330")))
      | Gadd -> 
      (match et1, et2 with |TGraph(n, e), TRec(_, _) -> [(et2, n); (t, TGraph(et2, e))]
                           |T(_), TRec(_,_) ->  [(et1, TGraph(et2, gen_new_type())); (t, et1)]
                           |T(_), T(_) -> [(et1, TGraph(et2, gen_new_type())); (t, et1)]
                           | _ -> raise(failwith("Error-- " ^ (string_of_type et1) ^ "," ^ (string_of_type et2) ^ " not valid types for Gadd")))    
      | Eadd -> 
      (match et1, et2 with |TGraph(n, e), TEdge(f) -> [(et2, e); (t, TGraph(n, et2))]
                           |T(_), TRec(_,_) | T(_), T(_) -> [(et1, TGraph(gen_new_type(), et2)); (t, et1)]
                           | _ -> raise(failwith("Error-- " ^ (string_of_type et1) ^ "," ^ (string_of_type et2) ^ " not valid graph for Eadd"))
      )
      | _ -> raise(failwith("error"))
     in
    (collect_expr ae1) @ (collect_expr ae2) @ opc (*opc appended at the rightmost since we apply substitutions right to left *)
  | AEdge(ae1, op, ae2, ae3, t) ->
    let et1 = type_of ae1 and et2 = type_of ae2 and et3 = type_of ae3 in 
    let opc = match op with
          | To | From | Dash ->
          (match et1, et2 with
          |TRec(_,_), TRec(_,_) -> [(et1, et2)]
          | _ -> raise(failwith("error: " ^ string_of_aexpr ae1 ^ " and " ^ string_of_aexpr ae2 ^ " must be nodes.")))
          | _ -> raise(failwith((string_of_op op) ^ " not an edge operator."))
     in 
     ignore(match et3 with
        | TRec(_,_) | T(_) -> ()
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


(*Step 3 of HM: unify constraints*)
and unify (constraints: (primitiveType * primitiveType) list) : substitutions =
(*       ignore(print_string "unifying\n"); *)
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

(*Are we handling lists right?*)
and substitute (u: primitiveType) (x: id) (t: primitiveType) : primitiveType =
  (*   print_string "substituting"; *)  
  match t with
  | TInt | TBool | TString | TFloat | TList(_) | TRec(_,_) | TChar | TEdge(_) | TGraph(_,_) | TVoid-> t 
  | T(c)  -> if c = x then u else t 
and apply (subs: substitutions) (t: primitiveType) : primitiveType =
  List.fold_right (fun (x, u) t -> substitute u x t) subs t

(*Step 4: Used in final application of substitutions*) 
and apply_expr (subs: substitutions) (ae: aexpr): aexpr =
(*     ignore(print_string "applying\n"); *)
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
and get_lval (ae: aexpr) =
  match ae with
  |ACall(str, _, _, _) 
  |AId(str, _) -> str 
  |ADot(ae, str, TRec(x, _)) -> map_id_rec x str
  |AItem(str, _, _) -> str

  (*is there a problem if this is a record??*)
  |_ -> raise(failwith("error: " ^ string_of_aexpr ae ^ " not a valid lvalue@534."))

(*Updates environment*)
and update_map (allenv: allenv) (a: astmt) : environment = 
(*   ignore(print_string ("updating map for " ^ (string_of_astmt a)));  *)
  let env, genv = allenv in
  match a with
  |AAsn(ae1, ae2, _,_) ->
    let id = map_id (get_lval ae1) in
(*     ignore(print_string (" updating " ^ id ^ " with type " ^ (string_of_type (type_of ae2)) ^ "\n"));  *)
    let t = type_of ae2 in
    let env = NameMap.add id t env in
    (* ignore(print_string(id ^ " is " ^ string_of_type (NameMap.find(id) env))); *)
    (update_map_expr id ae2 env)
  |AReturn(aexpr, _) -> env
  |AExpr(aexpr) -> env
  |AIf(_, a1, a2) -> env 
  |AFor(_, _, _, _) -> env
  |AWhile(_,_) -> env
  |AForin(_,_,_) -> env
and update_mapl (allenv: allenv) (alist : astmt list): environment =
  let rec helper (alist : astmt list) (env: environment) : environment =
  match alist with
  | [] -> env
  | hd :: tl -> 
    let env = update_map allenv hd in 
    helper tl env
  in let env, _ = allenv 
  in helper alist env
  
 (*Used when an expression itself changes the environment, i.e, in records or calls 
 that are secretly records. *)
  and update_map_expr (id: string) (aexpr: aexpr) (env: environment) : environment = 
    match aexpr with 
     | ARecord(et1, t) -> 
       update_map_derived id t env
(*      | ACall(id, _, astmts, t) -> 
       update_map_derived id t env *)
     | _ -> env

 and update_map_derived (id: string) (t: primitiveType) (env: environment) : environment = 
(*     ignore(print_string("update map derived\n")); *)
    (match t with
    TRec(tname, elist) -> 
      let rec helper l env = 
      (match l with
      |[] -> env
      |(field, fieldtype) :: tail -> 
      let env = NameMap.add (map_id (map_id_rec tname field)) fieldtype env in helper tail env)
      in helper elist env   
      |_ -> env)

(*Checks that return statements are consistent and returns the type for functions.*)
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

(*Applies the inferred type of formals from function statements to the functions themselves.*)
and infer_formals (f: string list) (env: environment):  (string * primitiveType) list=
  (*   ignore(print_string "Inferring formals!"); *)
  match f with
  |[] -> []
  | h :: tail -> 
    let fid = (map_id h) in
    let t = if NameMap.mem fid env
      then ( 
        NameMap.find fid env )
      else raise (failwith "formal not used") in (h,t) :: infer_formals tail env

(*Called from annotate_stmt, infers expressions inside statements.*)
and infer_expr (allenv: allenv) (e: expr): (aexpr)  =
  let annotated_expr = annotate_expr allenv e in
  let constraints = collect_expr annotated_expr in 
  let subs = unify constraints in
  let ret = apply_expr subs annotated_expr in ret

(*The calling method for this file. Infers all types for a func (statements, formals), and
outputs an annotated func. *)
and infer_func (allenv: allenv) (f: func) :  (afunc * genvironment)  =
  let env, genv = allenv in
  match f with
  |Fbody(decl, stmts) -> 
    ignore(match decl with Fdecl(fname, _)-> Stack.push fname callstack); (*set scope*)
    let ((_,genv), istmts) = infer_stmt_list allenv stmts (*infer the function statments*)
    in let ret_type = get_return_type istmts                       
    in match decl with
    |Fdecl(fname, formals) ->           (*add function to NameMap*) 
      if NameMap.mem fname genv
      then 
        let aformals = infer_formals formals env in   
        let genv = NameMap.add fname (ret_type, aformals, stmts) genv in 
        (ignore(Stack.pop callstack));
        AFbody(AFdecl(fname, aformals, ret_type), istmts),genv
      else raise (failwith "function not defined @ 412")
                                                       
