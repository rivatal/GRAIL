(* report errors found during code generation *)
exception Error of string

module L = Llvm
module A = Ast

module StringMap = Map.Make(String)

let translate (functions) = 
        (* define *)
        let context = L.global_context () in
        let the_module = L.create_module context "Grail"
        and i32_t = L.i32_type context
        and i8_t  = L.i8_type  context
        and i1_t  = L.i1_type  context
        and str_t = L.pointer_type (L.i8_type context)
        and float_t = L.float_type context
        and void_t= L.void_type context in
        
        let ltype_of_typ = function
                  A.TInt -> i32_t
                | A.TBool -> i1_t
                | A.TVoid -> void_t
                | A.TString -> str_t 
                | A.TFloat -> float_t in 


        (* Declare each global variable; remember its value in a map *)
        (* let global_vars = 
                let global_var m (t, n) = 
                        let init = L.const_int (ltype_of_typ t) 0
                        in StringMap.add n (L.define_global n init the_module) m in
                List.fold_left global_var StringMap.empty globals in
        *)

       (* Declare printf(), which the print built-in function will call *)
        let printf_t = L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
        let printf_func = L.declare_function "printf" printf_t the_module in

      (* Define each function (arguments and return type) so we can call it *) (** Fix the type thing here **)
        let function_decls =
            let function_decl m afunc=
            let name = afunc.A.fname
              and formal_types =
            Array.of_list (List.map (fun (_,t) -> ltype_of_typ t) afunc.A.formals)
              in let ftype = L.function_type (ltype_of_typ afunc.A.typ) formal_types in
              StringMap.add name (L.define_function name ftype the_module, afunc) m in
            List.fold_left function_decl StringMap.empty functions in
          
          (* Fill in the body of the given function *) (* FIX The Type Thing Here *)
        let build_function_body afunc =
            let (the_function, _) = StringMap.find afunc.A.fname function_decls in
            let builder = L.builder_at_end context (L.entry_block the_function) in

            (*let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder in*)
            
            (* Construct the function's "locals": formal arguments and locally
               declared variables.  Allocate each on the stack, initialize their
               value, if appropriate, and remember their values in the "locals" map *)
            (*let local_vars =
              let add_formal m (n, t) p = L.set_value_name n p;*)
        (*let local = L.build_alloca (ltype_of_typ t) n builder in
        ignore (L.build_store p local builder);
        StringMap.add n local m in
              let add_local m (n, t) =            
        let local_var = L.build_alloca (ltype_of_typ t) n builder
        in StringMap.add n local_var m in*)

              (*let formals = List.fold_left2 add_formal StringMap.empty afunc.A.formals
                  (Array.to_list (L.params the_function)) in
        (* Return the value for a variable or formal argument *)
        let lookup n = StringMap.find n local_vars in*)

        let int_ops op = 
          (match op with
            A.Add       -> L.build_add
            | A.Sub     -> L.build_sub
            | A.Mult    -> L.build_mul
            | A.Div     -> L.build_sdiv
            | A.Equal   -> L.build_icmp L.Icmp.Eq
            | A.Neq     -> L.build_icmp L.Icmp.Ne
            | A.Less    -> L.build_icmp L.Icmp.Slt
            | A.Leq     -> L.build_icmp L.Icmp.Sle
            | A.Greater -> L.build_icmp L.Icmp.Sgt
            | A.Geq     -> L.build_icmp L.Icmp.Sge
            | A.And     -> L.build_and
            | A.Or      -> L.build_or
            | _ -> raise (Failure "wrong operation applied to ints")
          )
        in

        let float_ops op = 
          (match op with
            A.FAdd       -> L.build_fadd
            | A.FSub     -> L.build_fsub
            | A.FMult    -> L.build_fmul
            | A.FDiv     -> L.build_fdiv
            | A.Equal   -> L.build_fcmp L.Fcmp.Oeq
            | A.Neq     -> L.build_fcmp L.Fcmp.One
            | A.Less    -> L.build_fcmp L.Fcmp.Ult
            | A.Leq     -> L.build_fcmp L.Fcmp.Ole
            | A.Greater -> L.build_fcmp L.Fcmp.Ogt
            | A.Geq     -> L.build_fcmp L.Fcmp.Oge
            | _ -> raise (Failure "wrong operation applied to floats")
          )
        in



        let rec aexpr builder = function
                A.AIntLit(i) -> L.const_int i32_t i
                | A.ABoolLit(b) -> L.const_int i1_t (if b then 1 else 0)
                | A.AStrLit(s) -> L.build_global_stringptr s "str" builder
                | A.ACharLit(c) -> L.const_int i8_t c
                | A.AFloatLit(f) -> L.const_float float_t f
               (* | A.List ->  why is List an expression, should not it be a data staructure?  *)
                | A.ACall ("print", [e], _) -> L.build_call printf_func [| (aexpr builder e) |] "printf" builder
                | A.Call (f, act) ->
                  let (fdef, fdecl) = StringMap.find f function_decls in
                  let actuals = List.rev (List.map (aexpr builder) (List.rev act)) in
                  let result = (match fdecl.A.typ with A.TVoid -> ""
                                            | _ -> f ^ "_result") in
                     L.build_call fdef (Array.of_list actuals) result builder
        (*        | A.Item ->
                | A.Subset ->
                | A.Dot ->  *)
                | A.Unop(op, e) -> let e' = aexpr builder e in
                        (match op with 
                                A.Neg -> L.build_neg
                                | A.Not -> L.build_not) e' "tmp" builder
                | A.ABinop (e1, op, e2) ->     let e1' = aexpr builder e1
                                              and e2' = aexpr builder e2 in
                                              (match e2 with 
                                              | A.AFloatLit _ -> (float_ops op) e1' e2' "tmp" builder
                                              | A.AId(_, A.TFloat) -> (float_ops op) e1' e2' "tmp" builder
                                              | A.ACall(_, _, A.TFloat) -> (float_ops op) e1' e2' "tmp" builder
                                              | _ -> (int_ops op) e1' e2' "tmp" builder                                              
                                            )
                | A.Noexpr -> L.const_int i32_t 0
              in

        
        (* Invoke "f builder" if the current block does not already 
           have a terminal (e.g., a branch). *)        
        let add_terminal builder f =
                match L.block_terminator (L.insertion_block builder) with
                        Some _ -> ()
                        | None -> ignore (f builder) in
        

        (* Build the code for the given statement; return the builder for
       the statement's successor *)

        let rec astmt builder = function
          A.AExpr(e) -> ignore (aexpr builder e); builder
        | A.AReturn(e, t) -> ignore(match t with
            A.TVoid -> L.build_ret_void builder
          | _ -> L.build_ret (aexpr builder e) builder); builder
        (*| A.AAsn(s, e, b) -> if b then let e' = aexpr builder e in ignore (L.build_store e' (lookup s) builder)
          else ; builder*)
        | A.AIf (predicate, then_stmt, else_stmt) ->
        let bool_val = aexpr builder predicate in
        let merge_bb = L.append_block context "merge" the_function in

        let then_bb = L.append_block context "then" the_function in
        add_terminal (List.fold_left astmt (L.builder_at_end context then_bb) then_stmt)
          (L.build_br merge_bb);

        let else_bb = L.append_block context "else" the_function in
          add_terminal (List.fold_left astmt (L.builder_at_end context else_bb) else_stmt)
          (L.build_br merge_bb);

        ignore (L.build_cond_br bool_val then_bb else_bb builder);
        L.builder_at_end context merge_bb

        | A.While (predicate, body) ->
        let pred_bb = L.append_block context "while" the_function in
         ignore (L.build_br pred_bb builder);

        let body_bb = L.append_block context "while_body" the_function in
        add_terminal (List.fold_left astmt (L.builder_at_end context body_bb) body)
        (L.build_br pred_bb);

        let pred_builder = L.builder_at_end context pred_bb in
        let bool_val = aexpr pred_builder predicate in

        let merge_bb = L.append_block context "merge" the_function in
        ignore (L.build_cond_br bool_val body_bb merge_bb pred_builder);
        L.builder_at_end context merge_bb


      | A.For (s1, e2, s3, body) -> List.fold_left astmt builder 
      [s1 ; A.While(e2, List.rev s3::(List.rev body))]
        in


        (* Build the code for each statement in the function *)
        let builder = List.fold_left astmt builder afunc.A.body in

        (* Add a return if the last block falls off the end *)
        add_terminal builder (match afunc.A.typ with
            A.TVoid -> L.build_ret_void
          | t -> L.build_ret (L.const_int (ltype_of_typ t) 0))
      in
            

List.iter build_function_body functions;
the_module



