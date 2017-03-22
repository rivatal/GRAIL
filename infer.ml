(* https://github.com/prakhar1989/type-inference/blob/master/infer.ml *)
open Ast

module NameMap = Map.Make(String)
type environment = primitiveType NameMap.t

(* Unknown type,  resolved type. eg.[(T, TInt); (U, TBool)] *)
type substitutions = (uId * primitiveType) list

let type_variable = ref (Char.code 'a')

(* generates a new unknown type placeholder.
 *    returns T(string) of the generated alphabet *)
let gen_new_type () =
    let c1 = !type_variable in
    incr type_variable; T(Char.escaped (Char.chr c1))
;;

let rec annotate_expr (e: expr) (env: environment) : aexpr =
    match e with
        | IntLit(n) -> AIntLit(n, TInt)
        | BoolLit(b) -> ABoolLit(b, TBool)
        | Id(x) -> if NameMap.mem x env
        then AId(x, NameMap.find x env)
        else raise (failwith "variable not defined")
        | Binop(e1, op, e2) ->
                let et1 = annotate_expr e1 env
                and et2 = annotate_expr e2 env
                and new_type = gen_new_type () in
                ABinop(et1, op, et2, new_type)

                (* returns the type of an annotated expression *)
        and type_of (ae: aexpr): primitiveType =
            match ae with
            | AIntLit(_, t) | ABoolLit(_, t) -> t
            | AId(_, t) -> t
            | ABinop(_, _, _, t) -> t
;;

let rec collect_expr (ae: aexpr) : (primitiveType * primitiveType) list =
    match ae with
        | AIntLit(_) | ABoolLit(_) -> []  (* no constraints to impose on literals *)
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
