(* report errors found during code generation *)
exception Error of string

module L = Llvm
module A = Ast

module StringMap = Map.Make(String)



(*
================================================================
        Main Codegen Function
================================================================
*)

let translate (functions) = 
        (* define *)
        let context = L.global_context () in
        let llm = Llvm_bitreader.parse_bitcode llctx customM in

        let the_module = L.create_module context "Grail"
        and i32_t = L.i32_type context
        and i8_t  = L.i8_type  context
        and i1_t  = L.i1_type  context
        and void_t= L.void_type context 
        and list_t = L.pointer_type (match L.type_by_name llm "struct.List" with
                                      None -> raise (Failure "struct.List doesn't defined.")
                                     | Some x -> x)
        in
        
        let ltype_of_typ = function
                  A.Int -> i32_t
                | A.Bool -> i1_t
                | A.Void -> void_t         
                | A.List _ -> list_t in

        let lconst_of_typ = function
                  A.Int -> L.const_int i32_t 0
                | A.Float -> L.const_int i32_t 1
                | A.Bool -> L.const_int i32_t 2
             (*   | A.String_t -> L.const_int i32_t 3
                | A.Node_t -> L.const_int i32_t 4
                | A.Graph_t -> L.const_int i32_t 5
                | A.Edge_t -> L.const_int i32_t 8     These data structures are not implemented yet *)
                | _ -> raise (Failure ("[Error] Type Not Found for lconst_of_typ.")) in


        (* Declare each global variable; remember its value in a map *)
        (* let global_vars = 
                let global_var m (t, n) = 
                        let init = L.const_int (ltype_of_typ t) 0
                        in StringMap.add n (L.define_global n init the_module) m in
                List.fold_left global_var StringMap.empty globals in
        *)




        (*
        ================================================================
          Declare printf(), which the print built-in function will call
        ================================================================
        *)
        let printf_t = L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
        let printf_func = L.declare_function "print" printf_t the_module in


        (*
        ================================================================
          List (Array)
        ================================================================
        *)
                
        let create_list_t  = L.function_type list_t [| i32_t |] in
        let create_list_f  = L.declare_function "createList" create_list_t the_module in
        let create_list typ llbuilder =
          let actuals = [|lconst_of_typ typ|]in (
            L.build_call create_list_f actuals "createList" llbuilder
          ) in

        let rec add_multi_elements_list l_ptr typ llbuilder = function
          | [] -> l_ptr
          | h :: tl -> add_multi_elements_list (add_list (cast_float h typ llbuilder) l_ptr llbuilder) typ llbuilder tl
        in      








        (*
        ================================================================
          Function Declaration
        ================================================================
        *)
        (* Define each function (arguments and return type) so we can call it *) (** Fix the type thing here **)
        let function_decls =
            let function_decl m funcs =
            let name = funcs.A.fname
              and formal_types =
            Array.of_list (List.map (fun (t,_) -> ltype_of_typ t) funcs.A.formals)
              in let ftype = L.function_type (ltype_of_typ fdecl.A.typ) formal_types in
              StringMap.add name (L.define_function name ftype the_module, fdecl) m in
            List.fold_left function_decl StringMap.empty functions in
          
          (* Fill in the body of the given function *) (* FIX The Type Thing Here *)
        let build_function_body fdecl =
            let (the_function, _) = StringMap.find fdecl.A.fname function_decls in
            let builder = L.builder_at_end context (L.entry_block the_function) in

            let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder in
            
            (* Construct the function's "locals": formal arguments and locally
               declared variables.  Allocate each on the stack, initialize their
               value, if appropriate, and remember their values in the "locals" map *)
            let local_vars =
              let add_formal m (t, n) p = L.set_value_name n p;
        let local = L.build_alloca (ltype_of_typ t) n builder in
        ignore (L.build_store p local builder);
        StringMap.add n local m in

              let add_local m (t, n) =            
        let local_var = L.build_alloca (ltype_of_typ t) n builder
        in StringMap.add n local_var m in

              let formals = List.fold_left2 add_formal StringMap.empty fdecl.A.formals
                  (Array.to_list (L.params the_function)) in
              List.fold_left add_local formals fdecl.A.locals in

        (* Return the value for a variable or formal argument *)
        let lookup n = try StringMap.find n local_vars in

        let rec expr builder = function
                A.IntLit i -> L.const_int i32_t i
                | A.BoolLit b -> L.const_int i1_t (if b then 1 else 0)
                | A.StrLit s -> L.pointer_type i8_t s
                | A.CharLit c -> L.const_int i8_t c
             (*   | A.FloatLit f -> *)
                | A.Id a -> L.build_load (lookup a) a builder (* why this format *)
                | A.List l ->  
                    let from_expr_typ_to_list_typ = function 
                         A.IntLit -> A.List
                        | A.BoolLit -> A.List  (* need to add options if have list of floats, nodes, edges and graphs *)
                        | A.StrLit -> A.List
                        | A.CharLit -> A.List
                        | _ -> A.List (* how are we gonna take care of void, id, list of list? *)
                    in

                    let rec check_float_typ = function
                         [] -> A.Int_t
                         | hd::ls -> if (snd(expr builder hd)) == A.Float then A.Float else check_float_typ ls in

                    let list_typ = snd (expr builder (List.hd ls)) in (* the second element is type since 'aexpr * primitiveType' *)
                    let list_typ = if list_typ == A.Int then check_float_typ ls else list_typ in

                    (* converting a list of nodes to a graph, if did not understand wrong *)
                    let list_conversion el =
                          let (e_val, e_typ) = expr builder el in
                          ( match e_typ with 
                              _ -> (e_val, e_typ)
                          )
                        (*  (   match e_typ with
                            | A.Node_t when list_typ = A.Graph_t -> (
                                let gh = create_graph builder in (
                                    ignore(graph_add_node gh e_val builder);
                                    (gh, A.Graph_t)
                                )
                              )
                            | _ -> (e_val, e_typ)
                          )
                        *)
                      in 



                    (* create a new list here *)
                    let l_ptr_type = (create_list list_typ builder, from_expr_typ_to_list_typ list_typ) in
                    (* then add all initial values to the list *)
                    add_multi_elements_list (fst l_ptr_type) list_typ builder (List.map fst (List.map list_conversion ls)), (snd l_ptr_type)








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
        

        (* Build the code for the given statement; return the builder for
       the statement's successor *)
        let rec stmt builder = function
          A.Expr e -> ignore (expr builder e); builder


        (* Build the code for each statement in the function *)
        let builder = stmt builder (A.Block fdecl.A.body) in

        (* Add a return if the last block falls off the end *)
        add_terminal builder (match fdecl.A.typ with
            A.Void -> L.build_ret_void
          | t -> L.build_ret (L.const_int (ltype_of_typ t) 0))
      in
            

List.iter build_function_body functions;
the_module



