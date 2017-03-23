(* https://github.com/prakhar1989/type-inference/blob/master/infer.ml *)
open Ast
open Astutils   
module NameMap = Map.Make(String)
type environment = primitiveType NameMap.t

(* Unknown type,  resolved type. eg.[(T, TInt); (U, TBool)] *)
type substitutions = (id * primitiveType) list
let type_variable = ref (Char.code 'a')

(* generates a new unknown type placeholder.
 *    returns T(string) of the generated alphabet *)
let gen_new_type () =
    let c1 = !type_variable in
    incr type_variable; T(Char.escaped (Char.chr c1))


let rec annotate_expr (e: expr) (env: environment) : aexpr =
    match e with
        | IntLit(n) -> AIntLit(n, TInt)
        | BoolLit(b) -> ABoolLit(b, TBool)
        | StrLit(s) -> AStrLit(s,TString)
        | Id(x) -> if NameMap.mem x env
        then AId(x, NameMap.find x env)
        else raise (failwith "variable not defined")
        | Binop(e1, op, e2) ->
          let et1 = annotate_expr e1 env
          and et2 = annotate_expr e2 env
          and new_type = gen_new_type () in
          ABinop(et1, op, et2, new_type)
        | Fun(id, e) ->
          let ae = annotate_expr e env in
          let t = NameMap.find id env in
          AFun(id, ae, TFun(t, gen_new_type ()))
        (* returns the type of an annotated expression *)
        and type_of (ae: aexpr): primitiveType =
            match ae with
            | AIntLit(_, t) | ABoolLit(_, t) | AStrLit(_,t) -> t
            | AId(_, t) -> t
            | ABinop(_, _, _, t) -> t
            | AFun(_, _, t) -> t


(*Make an annotate function/statement.
  Annotate statements and recursively annotate its expressions.
*)
let rec annotate_stmt (e: stmt) (env: environment) : astmt =
  match e with
    | Asn(id, expr, switch) -> 
        let aexpr = annotate_expr expr env
        and t = 
          if NameMap.mem id env
          then NameMap.find id env
         else raise (failwith "variable not defined")
     in AAsn(id, aexpr, switch, t)
    | Return(expr) ->
      let aexpr = annotate_expr expr env in AReturn(aexpr, gen_new_type())
    and type_of_stmt (a: astmt): primitiveType = 
       match a with
      | AAsn(_, _, _, t) -> t
      | AReturn(_, t) -> t

(* Annotate a statement list *)
let  rec annotate_stmt_list(st : stmt list ) (env : environment) : astmt list =
    match st with 
        | [] -> []
        | hd :: tl -> (annotate_stmt hd env) ::  (annotate_stmt_list  tl env)

let rec collect_expr (ae: aexpr) : (primitiveType * primitiveType) list =
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
                    (* opc appended at the rightmost since we apply substitutions right to left *)
                    (collect_expr ae1) @ (collect_expr ae2) @ opc
         | AFun(id, ae, t) -> 
           (match t with
            | TFun(idt, ret_type) -> (collect_expr ae) @ [(type_of ae, ret_type)]
            | _ -> raise (failwith "not a function"))
      
let collect_stmt (a: astmt) : (primitiveType * primitiveType) list =
   match a with
    | AAsn(id, aexpr, switch, t) ->
      collect_expr aexpr @ [(type_of aexpr , t)]
    | AReturn(aexpr, t) ->
      collect_expr aexpr @ [(type_of aexpr , t)]

(* Collect statement list *)
let rec collect_stmt_list (astlist: astmt list) : (primitiveType * primitiveType) list = 
    match astlist with 
    | [] -> []
    | hd :: tl -> (collect_stmt hd) @ collect_stmt_list tl 



let rec substitute (u: primitiveType) (x: id) (t: primitiveType) : primitiveType =
  match t with
  | TInt | TBool | TString -> t
  | T(c) -> if c = x then u else t
  | TFun(t1, t2) -> TFun(substitute u x t1, substitute u x t2)


let apply (subs: substitutions) (t: primitiveType) : primitiveType =
  List.fold_right (fun (x, u) t -> substitute u x t) subs t
;;

let rec unify (constraints: (primitiveType * primitiveType) list) : substitutions =
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
  
  (* This case is particularly useful when you are calling a function that returns a function *)
  | TFun(a, b), TFun(x, y) -> unify [(a, x); (b, y)]
  | _ -> raise (failwith "mismatched types")
;;


(* applies a final set of substitutions on the annotated expr *)
let rec apply_expr (subs: substitutions) (ae: aexpr): aexpr =
  match ae with
  | ABoolLit(b, t) -> ABoolLit(b, apply subs t)
  | AIntLit(n, t) -> AIntLit(n, apply subs t)
  | AStrLit(s,t) -> AStrLit(s, apply subs t)
  | AId(s, t) -> AId(s, apply subs t)
  | ABinop(e1, op, e2, t) -> ABinop(apply_expr subs e1, op, apply_expr subs e2, apply subs t)
  | AFun(id, e, t) -> AFun(id, apply_expr subs e, apply subs t)
;;

let rec apply_stmt (subs: substitutions) (a: astmt): astmt = 
  match a with
  | AAsn(id, aexpr, switch, t) -> 
    AAsn(id, apply_expr subs aexpr, switch, apply subs t) 
  | AReturn(aexpr, t) ->
     AReturn(apply_expr subs aexpr, apply subs t) 


let rec apply_stmt_list (subs:substitutions) (astlist : astmt list) : astmt list = 
    match astlist with 
    | [] -> []
    | hd :: tl -> (apply_stmt subs hd) ::  apply_stmt_list subs tl

let print_constraints (x,y) =
    print_endline (string_of_type(x) ^ ":" ^ string_of_type(y)) 


let rec update_map (alist : astmt list) (env: environment) =
  match alist with
    | [] -> ()
    | hd :: tl ->
      match hd with
      AAsn(id, aexpr, _, t) ->
      ignore(NameMap.add id (T(string_of_type t)) env);
      match aexpr with
      | AId(s, t) -> 
        ignore(NameMap.add s (T(string_of_type t)) env);
      | AFun(id, _, t) -> 
        ignore(NameMap.add id (T(string_of_type t)) env);
      ignore(update_map tl env)

let infer_stmt_list (env: environment) (e: stmt list) : astmt list =
  let annotated_stmtlist = annotate_stmt_list e env in
  let constraints = collect_stmt_list annotated_stmtlist in 
    (*List.iter print_constraints constraints;*)
    let subs = unify constraints in
    (* reset the type counter after completing inference *)
    type_variable := (Char.code 'a');
    let retlist = apply_stmt_list subs annotated_stmtlist
    in 
        update_map retlist env;
        retlist

let rec grab_returns (r: astmt list) : primitiveType list =
   match r with
    | [] -> []
    | h :: tail -> 
        match h with
          |AReturn(_, t) -> t :: grab_returns tail
          | _ -> grab_returns tail


(*Checks that return statements are consistent and returns the type*)
let get_return_type(r: astmt list) : primitiveType =
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


let rec infer_formals (f: string list) (env: environment):  primitiveType list=
    match f with
    |[] -> []
    | h :: tail -> 
    let t = if NameMap.mem h env
            then NameMap.find h env
             else raise (failwith "formal not used") in t :: infer_formals tail env

(*Infer the type of the statements in the function*)
let infer_func (f: func) (env: environment): afunc =
  match f with
        |Fbody(decl, stmts) ->  
        let astmts = 
           infer_stmt_list env stmts
        in let ret_type =
            get_return_type astmts            
        in  
            match decl with
            |Fdecl(name, formals) ->           (*annotate the formals*)
                if NameMap.mem name env
                then ignore(NameMap.add name (T(string_of_type ret_type)) env)
                else raise (failwith "function not defined");
                let aformals = infer_formals formals env  
                in AFbody(AFdecl(name, aformals, ret_type), astmts)

(* Annotate an expression list *)
let rec annotate_expr_list(e : expr list ) (env : environment) : aexpr list =
    match e with 
        | [] -> []
        | hd :: tl -> ((annotate_expr hd env) :: (annotate_expr_list tl env))
