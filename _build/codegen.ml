(* report errors found during code generation *)
exception Error of string

module L = Llvm
module A = Ast
module C = Char

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
  and void_t= L.void_type context
  in

  let rec ltype_of_typ = function
      A.TInt -> i32_t
    | A.TChar -> i8_t
    | A.TBool -> i1_t
    | A.TVoid -> void_t
    | A.TString -> str_t 
    | A.TFloat -> float_t
    | A.TList t -> L.struct_type context [|L.pointer_type (ltype_of_typ t); i32_t|]
    (*| A.TList t -> L.pointer_type (ltype_of_typ t)*) in 


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

    let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder in
    let float_format_str = L.build_global_stringptr "%f\n" "fmt" builder in

    (* Construct the function's "locals": formal arguments and locally
       declared variables.  Allocate each on the stack, initialize their
       value, if appropriate, and remember their values in the "locals" map *)
    let local_vars =
      let add_formal m (n, t) p = L.set_value_name n p;
        let local = L.build_alloca (ltype_of_typ t) n builder in
        ignore (L.build_store p local builder);
        StringMap.add n local m in  

      List.fold_left2 add_formal StringMap.empty afunc.A.formals (Array.to_list (L.params the_function)) 
    in 
    let lookup n map = try StringMap.find n map
      with Not_found -> raise (Failure ("undeclared variable " ^ n))
    in let int_ops op = 
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
         A.Fadd       -> L.build_fadd
       | A.Fsub     -> L.build_fsub
       | A.Fmult    -> L.build_fmul
       | A.Fdiv     -> L.build_fdiv
       | A.Equal   -> L.build_fcmp L.Fcmp.Oeq
       | A.Neq     -> L.build_fcmp L.Fcmp.One
       | A.Less    -> L.build_fcmp L.Fcmp.Ult
       | A.Leq     -> L.build_fcmp L.Fcmp.Ole
       | A.Greater -> L.build_fcmp L.Fcmp.Ogt
       | A.Geq     -> L.build_fcmp L.Fcmp.Oge
       | _ -> raise (Failure "wrong operation applied to floats")
      )
    in

    (*let list_ops op =
    (mach op with
      A.Ladd -> add_to_list
    )*)

    let get_list_type t = (*quick utility function to map TList to the list's type*)
    (match t with
      A.TList x -> x
    |  _ -> raise(Failure "problem typing lists"))
  in

  let rec assign_array ar els n builder = (*stores elements, starting with element n in ar, returns ar*)
    match els with
      [] -> ar
    | e::tl -> let p = L.build_in_bounds_gep ar [|(L.const_int i32_t n)|] "ptr" builder in
                      ignore(L.build_store e p builder); assign_array ar tl (n+1) builder
    in


    let rec aexpr builder local_var_map = function
        A.AIntLit(i, _) -> L.const_int i32_t i
      | A.ABoolLit(b, _) -> L.const_int i1_t (if b then 1 else 0)
      | A.AStrLit(s, _) -> L.build_global_stringptr s "str" builder
      | A.ACharLit(c, _) -> L.const_int i8_t (C.code c)
      | A.AFloatLit(f, _) -> L.const_float float_t f
      | A.AId(s,_) -> L.build_load (lookup s local_var_map) s builder 
      | A.AList(l, t) ->  let list_typ = get_list_type t and els = List.map (aexpr builder local_var_map) l in 
        let struct_var = L.build_alloca (ltype_of_typ t) "strct" builder in
        let ar_var = L.build_array_alloca (ltype_of_typ list_typ) (L.const_int i32_t (List.length l)) "lst" builder in
        let init_list = assign_array ar_var els 0 builder in
        let p0 = L.build_struct_gep struct_var 0 "p0" builder and p1 = L.build_struct_gep struct_var 1 "p1" builder in
        ignore(L.build_store init_list p0 builder); ignore(L.build_store (L.const_int i32_t (List.length l)) p1 builder); L.build_load struct_var "lst" builder
      | A.AItem(s, e, t) -> let strct = lookup s local_var_map in let arp = L.build_struct_gep strct 0 "tmp" builder in
                            let ar = L.build_load arp "tmpar" builder and ad = aexpr builder local_var_map e in
                            let p = L.build_in_bounds_gep ar [|ad|] "ptr" builder in L.build_load p "item" builder
      | A.ACall ("print", [e], _, _) -> L.build_call printf_func [| (aexpr builder local_var_map e) |] "printf" builder
      | A.ACall("printint", [e], _, _) | A.ACall ("printbool", [e], _, _) -> L.build_call printf_func [| int_format_str ; (aexpr builder local_var_map e) |] "printf" builder
      | A.ACall("printfloat", [e], _, _) -> L.build_call printf_func [| float_format_str ; (aexpr builder local_var_map e) |] "printf" builder
      | A.ACall (f, act, _, _) ->
        let (fdef, fdecl) = StringMap.find f function_decls in
        let actuals = List.rev (List.map (aexpr builder local_var_map) (List.rev act)) in
        let result = (match fdecl.A.typ with A.TVoid -> ""
                                           | _ -> f ^ "_result") in
        L.build_call fdef (Array.of_list actuals) result builder
      | A.AUnop(op, e, t) ->
            let e' = aexpr builder local_var_map e in
           (match op with
            A.Neg     -> L.build_neg
           | A.Not     -> L.build_not) e' "tmp" builder
      | A.ABinop (e1, op, e2, t) ->     let e1' = aexpr builder local_var_map e1
        and e2' = aexpr builder local_var_map e2 in
        (match t with 
         | A.TFloat _ -> (float_ops op) e1' e2' "tmp" builder
         | _ -> (int_ops op) e1' e2' "tmp" builder                                              
        )
        (* Edge, Graph, Node, Record *)
        | A.ANoexpr _ -> L.const_int i32_t 0

    (* Invoke "f builder" if the current block does not already 
       have a terminal (e.g., a branch). *)        
    in  let add_terminal (builder, local_var_map) f =
          match L.block_terminator (L.insertion_block builder) with
            Some _ -> ()
          | None -> ignore (f builder) 

    (* Build the code for the given statement; return the builder for
       the statement's successor *)

    in let rec astmt (builder, local_var_map) = function
          A.AExpr(e) -> ignore (aexpr builder local_var_map e); (builder, local_var_map)
        | A.AReturn(e, t) -> ignore(match t with
              A.TVoid -> L.build_ret_void builder
            | _ -> L.build_ret (aexpr builder local_var_map e) builder); (builder, local_var_map)
        | A.AAsn(s, e, b, t) -> 
          let add_local m (t,n) =            
            let local_var = L.build_alloca (ltype_of_typ t) n builder
            in StringMap.add n local_var m in
          (match s with
            A.AId(name, typ) -> let local_var_map = if StringMap.mem name local_var_map then local_var_map else add_local local_var_map (t,name) in 
           let e' = aexpr builder local_var_map e in ignore (L.build_store e' (lookup name local_var_map) builder); (builder, local_var_map)
          | A.AItem(name, adr, typ) -> let e' = aexpr builder local_var_map e and 
            arp = L.build_struct_gep (lookup name local_var_map) 0 "tmp" builder and ad = aexpr builder local_var_map adr in
            let ar = L.build_load arp "tmpar" builder in
            let p = L.build_in_bounds_gep ar [|ad|] "ptr" builder in ignore(L.build_store e' p builder); (builder, local_var_map))
        | A.AIf (predicate, then_stmt, else_stmt) ->
          let bool_val = aexpr builder local_var_map predicate in
          let merge_bb = L.append_block context "merge" the_function in

          let then_bb = L.append_block context "then" the_function in
          add_terminal (List.fold_left astmt (L.builder_at_end context then_bb, local_var_map) then_stmt)
            (L.build_br merge_bb);

          let else_bb = L.append_block context "else" the_function in
          add_terminal (List.fold_left astmt ((L.builder_at_end context else_bb), local_var_map) else_stmt)
            (L.build_br merge_bb);

          ignore (L.build_cond_br bool_val then_bb else_bb builder);
          (L.builder_at_end context merge_bb, local_var_map)

        | A.AWhile (predicate, body) ->
          let pred_bb = L.append_block context "while" the_function in
          ignore (L.build_br pred_bb builder);

          let body_bb = L.append_block context "while_body" the_function in
          add_terminal (List.fold_left astmt ((L.builder_at_end context body_bb), local_var_map) body)
            (L.build_br pred_bb);

          let pred_builder = L.builder_at_end context pred_bb in
          let bool_val = aexpr pred_builder local_var_map predicate in

          let merge_bb = L.append_block context "merge" the_function in
          ignore (L.build_cond_br bool_val body_bb merge_bb pred_builder);
          (L.builder_at_end context merge_bb, local_var_map)


        | A.AFor (s1, e2, s3, body) -> List.fold_left astmt (builder, local_var_map)
                                         [s1 ; A.AWhile(e2, List.rev (s3::(List.rev body)))]
                                         (* Build the code for each statement in the function *)

        | A.AForin (e1, e2, body) -> 
        let ind = (match e1 with A.AId(s, t) -> (s, t)) in let lst = L.build_alloca (ltype_of_typ (A.TList (snd ind))) "lst" builder
        in ignore(L.build_store (aexpr builder local_var_map e2) lst builder);
        let elvar = L.build_alloca (ltype_of_typ (snd ind)) (fst ind) builder in
        let local_var_map = StringMap.add (fst ind) elvar local_var_map in
        let ar = L.build_load (L.build_struct_gep lst 0 "tmp" builder) "ar" builder and endlst = L.build_load (L.build_struct_gep lst 1 "tmp" builder) "end" builder in
        let elind = L.build_alloca i32_t "ind" builder in ignore(L.build_store (L.const_int i32_t 0) elind builder);
        
        let pred_bb = L.append_block context "while" the_function in
        ignore (L.build_br pred_bb builder);

        let body_bb = L.append_block context "while_body" the_function in
        let body_builder = L.builder_at_end context body_bb in
        let p = L.build_in_bounds_gep ar [|(L.build_load elind) "i" body_builder|] "ptr" body_builder in ignore(L.build_store (L.build_load p "tmp" body_builder) elvar body_builder);
        let (endbody_builder, new_local_var_map) = (List.fold_left astmt ((L.builder_at_end context body_bb), local_var_map) body) in
        ignore(L.build_store (L.build_add (L.build_load elind "tmp" endbody_builder) (L.const_int i32_t 1) "inc" endbody_builder) elind endbody_builder);
        add_terminal (endbody_builder, new_local_var_map) (L.build_br pred_bb);

        let pred_builder = L.builder_at_end context pred_bb in
        let bool_val = L.build_icmp L.Icmp.Slt (L.build_load elind "tmp" pred_builder) endlst "comp" pred_builder in

        let merge_bb = L.append_block context "merge" the_function in
        ignore (L.build_cond_br bool_val body_bb merge_bb pred_builder);
        (L.builder_at_end context merge_bb, local_var_map)

          


    in let (builder,local_vars) = List.fold_left 
           astmt (builder,local_vars) afunc.A.body 
    in
    (* Add a return if the last block falls off the end *)
    add_terminal (builder, local_vars) (match afunc.A.typ with
          A.TVoid -> L.build_ret_void
        | t -> L.build_ret (L.const_int (ltype_of_typ t) 0))
  in


  List.iter build_function_body functions;
  the_module


