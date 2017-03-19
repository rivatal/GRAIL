(* https://github.com/prakhar1989/type-inference/blob/master/infer.ml *)
open Ast

module NameMap = Map.Make(String)
type environment = primitiveType NameMap.t

(* Unknown type,  resolved type. eg.[(T, TNum); (U, TBool)] *)
type substitutions = (id * primitiveType) list

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


