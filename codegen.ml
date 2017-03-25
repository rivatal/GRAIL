(* report errors found during code generation *)
exception Error of string


module L = Llvm
module A = Ast

module StringMap = Map.Make(String)

let translate (globals, functions) = 
        (* define *)
        let context = L.global_context () in
        let the_module = L.create_module context "Grail"
        and i32_t = L.i32_type context
        and i8_t  = L.i8_type  context
        and i1_t  = L.i1_type  context
        and void_t= L.void_type context in
        
        let ltype_of_typ = function
                  A.Int -> i32_t
                | A.Bool -> i1_t
                | A.Void -> void_t in        

        (* Declare each global variable; remember its value in a map *)
        (* let global_vars = 
                let global_var m (t, n) = 
                        let init = L.const_int (ltype_of_typ t) 0
                        in StringMap.add n (L.define_global n init the_module) m in
                List.fold_left global_var StringMap.empty globals in
        *)

       (* Declare printf(), which the print built-in function will call *)
        let printf_t = L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
        let printf_func = L.declare_function "print" printf_t the_module in

       (* Define each function (arguments and return type) so we can call it *)
       (* let function_decls = ... *)

       (* Fill in the body of the given function *)
       (* let build_function_body fdecl = ... *)

       (* Construct the function's "locals": formal arguments and locally
          declared variables. Allocate each of the stack, initialize their
          value, if appropriate, and remember their values in the "local" map *)



        (* Return the value for a variable or formal argument *)
        let lookup n = try StringMap.find n local_vars
                        with Not_found -> StringMap.find n global_vars
        in

        let rec expr builder = function
                A.IntLit i -> L.const_int i32_t i
                | A.BoolLit b -> L.const_int i1_t (if b then 1 else 0)
              (*  | A.StrLit s -> 
                | A.CharLit c -> 
                | A.FloatLit f -> *)
                | A.Id a -> L.build_load (lookup a) a builder (* why this format *)
               (* | A.List ->  why is List an expression, should not it be a data staructure?  *)
                | A.Call ("print", [e]) -> L.build_call printf_func [| int_format_str ; (expr builder e) |] "printf" builder
        (*        | A.Item ->
                | A.Subset ->
                | A.Dot ->  *)
                | A.Unop(op, e) -> let e' = expr builder e in
                        (match op with 
                                A.Neg -> L.build_neg
                                | A.Not -> L.build_not) e' "tmp" builder
                | A.Binop (e1, op, e2) -> let e1' = expr builder e1
                                          let e2' = expr builder e2 in
                                          (match op with
                                                A.Add -> L.build_add
                                                | A.Sub -> L.build_sub
                                                | A.Mult -> L.build_mul
                                                | A.Div -> L.build_sdiv
                                                | A.Equal -> L.build_icmp L.Icmp.Eq
                                                | A.Neq -> L.build_icmp L.Icmp.Ne
                                                | A.Less -> L.build_icmp L.Icmp.Slt
                                                | A.Leq -> L.build_icmp L.Icmp.Sle
                                                | A.Greater -> L.build_icmp L.Icmp.Sgt
                                                | A.Geq -> L.build_icmp L.Icmp.Sge
                                                | A.And -> L.build_and
                                                | A.Or -> L.build_or
                                                (* what are In, Fadd, Fsub, Fmult, Fdiv, Gadd, Eadd*)
                                          ) e1' e2' "tmp" builder
                (* Edge, Graph, Node, Record *)
                | A.Noexpr -> L.const_int i32_t 0

        
        (* Invoke "f builder" if the current block does not already 
           have a terminal (e.g., a branch). *)        
        let add_terminal builder f =
                match L.block_terminator (L.insertion_block builder) with
                        Some _ -> ()
                        | None -> ignore (f builder) in
        
        (* stmt section below *)
        let rec stmt builder = function
                A.Expr e -> ignore (expr builder e); builder        
        
        
        (* Build the code for each statement in the function *)
        (* let builder = stmt builder (A.Block fdecl.A.body) in *)        

        (* Add a return if the last block falls off the end *)
        (* add_terminal builder (match fdecl.A.typ with
                A.Void -> L.build_ret_void
                | t -> L.build_ret (L.const_int (ltype_of_typ t) 0))
        *)
in

List.iter build_function_body functions;
the_module



