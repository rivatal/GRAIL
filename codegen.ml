(* report errors found during code generation *)
exception Error of string

module L = Llvm
module A = Ast
module C = Char

module StringMap = Map.Make(String)
module TypeMap = Map.Make(String)

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
  and pointer_t = L.pointer_type
  in 
  let tymap = (ref TypeMap.empty) 
  in let rec ltype_of_typ = function
      A.TInt -> i32_t
    | A.TChar -> i8_t
    | A.TBool -> i1_t
    | A.TVoid -> void_t
    | A.TString -> str_t 
    | A.TFloat -> float_t
    | A.TList t -> L.struct_type context [|L.pointer_type (ltype_of_typ t); i32_t|]
    | A.TRec(tany,tlist) ->
            let tname = (match tany with A.T s -> s | _ -> raise(Failure "the typer somehow gave us wrong input")) in
            let struct_name = ("struct."^tname) in 
            if TypeMap.mem struct_name !tymap 
            then 
               TypeMap.find struct_name !tymap
            else
                let record_t = L.named_struct_type context struct_name in 
			    let ret_types = 
			    Array.of_list(List.map (fun (_,t) -> ltype_of_typ t) tlist) 
                in L.struct_set_body record_t ret_types false;
                tymap := TypeMap.add ("struct."^tname) record_t !tymap;
                record_t
    |A.TEdge(tany,trec1,trec2) ->
           let tname = (match tany with A.T s -> s | _ -> raise(Failure "the typer somehow gave us wrong input")) in
           let struct_name = ("struct."^tname) in 
           if TypeMap.mem struct_name !tymap 
           then 
               TypeMap.find struct_name !tymap
           else
           let struct_name = ("struct."^tname) in 
           let edge_t = L.named_struct_type context struct_name in 
           let ret_types = 
                          [  pointer_t (ltype_of_typ trec1);
                             pointer_t (ltype_of_typ trec1);
                             ltype_of_typ A.TBool;
                             ltype_of_typ trec2;
                           ] 
           in let all_ret_types = Array.of_list(ret_types)
           in L.struct_set_body edge_t all_ret_types false;
           tymap := TypeMap.add ("struct."^tname) edge_t !tymap;
           edge_t
    | A.TGraph(tany, nt, et) -> 
           let tname = (match tany with A.T s -> s | _ -> raise(Failure "the typer somehow gave us wrong input")) in
           let struct_name = ("struct."^tname) in 
           if TypeMap.mem struct_name !tymap 
           then 
               TypeMap.find struct_name !tymap
           else
           let struct_name = ("struct."^tname) in 
           let graph_t = L.named_struct_type context struct_name in 
           let ereltyp = (match et with A.TEdge(_, _, rel) -> rel | _ -> raise(Failure "wrong edge type")) in
           let ret_types = [| (ltype_of_typ (A.TList nt)); (ltype_of_typ (A.TList et)); (ltype_of_typ ereltyp)|] 
           in L.struct_set_body graph_t ret_types false;
           tymap := TypeMap.add ("struct."^tname) graph_t !tymap;
           graph_t
    | _ -> raise(Failure "provided a bad type")
    in   
 (* Declare printf(), which the print built-in function will call *)
  let printf_t = L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_func = L.declare_function "printf" printf_t the_module in

 (* Declare sample_display(),for displaying a sample graph *)
  let display_t = L.function_type i32_t [| i32_t |] in
  let display_func = L.declare_function "sample_display" display_t the_module in

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
    in

        (* Invoke "f builder" if the current block does not already 
       have a terminal (e.g., a branch). *)        
    let add_terminal builder f =
          match L.block_terminator (L.insertion_block builder) with
            Some _ -> ()
          | None -> ignore (f builder) 
    in

    let get_list_type t = (*quick utility function to map TList to the list's type*)
    (match t with
      A.TList x -> x
    |  _ -> raise(Failure "problem typing lists"))
  in

  let get_graph_types t = (*maps TGraph to the node and  types*)
  (match t with
    A.TGraph(_, nt, et) -> (nt, et)
  | _ -> raise(Failure "not a graph")
  )
in 

  let rec compare e1 e2 t builder =
    (match t with
    A.TInt | A.TChar | A.TBool -> (L.build_icmp L.Icmp.Eq e1 e2 "tmp" builder, builder)
    | A.TFloat -> (L.build_fcmp L.Fcmp.Oeq e1 e2 "tmp" builder, builder)
    | A.TRec(_, fields) -> let rec1 = L.build_alloca (ltype_of_typ t) "strct" builder in ignore(L.build_store e1 rec1 builder);
        let rec2 = L.build_alloca (ltype_of_typ t) "strct" builder in ignore(L.build_store e2 rec2 builder);
        compare_fields 0 fields rec1 rec2 builder
    | A.TEdge(_, trec1, trec2) -> let ed1 = L.build_alloca (ltype_of_typ t) "edge" builder in ignore(L.build_store e1 ed1 builder);
        let ed2 = L.build_alloca (ltype_of_typ t) "edge" builder in ignore(L.build_store e2 ed2 builder);
        let (fromcomp, builder) = compare (L.build_load (L.build_struct_gep ed1 0 "tmp" builder) "val" builder) 
                                          (L.build_load (L.build_struct_gep ed2 0 "tmp" builder) "val" builder) trec1 builder in
        let (tocomp, builder) = compare (L.build_load (L.build_struct_gep ed1 1 "tmp" builder) "val" builder)
                                        (L.build_load (L.build_struct_gep ed2 1 "tmp" builder) "val" builder)  trec1 builder in
        let (dircomp, builder) = compare (L.build_load (L.build_struct_gep ed1 2 "tmp" builder) "val" builder)
                                          (L.build_load (L.build_struct_gep ed2 2 "tmp" builder) "val" builder) A.TBool builder in
        let (relcomp, builder) = compare (L.build_load (L.build_struct_gep ed1 3 "tmp" builder) "val" builder)
                                          (L.build_load (L.build_struct_gep ed1 3 "tmp" builder) "val" builder) trec2 builder in
        (L.build_mul fromcomp (L.build_mul tocomp (L.build_mul dircomp relcomp "tmp" builder) "tmp" builder) "tmp" builder, builder)

    | A.TGraph(_, ntyp, etyp) -> let ereltyp = (match etyp with A.TEdge(_, _, rel) -> rel | _ -> raise(Failure "wrong edge type")) in
        let g1 = L.build_alloca (ltype_of_typ t) "graph" builder in ignore(L.build_store e1 g1 builder);
        let g2 = L.build_alloca (ltype_of_typ t) "graph" builder in ignore(L.build_store e2 g2 builder);
        let (nodescomp, builder) = compare (L.build_load (L.build_struct_gep g1 0 "tmp" builder) "val" builder) 
                                          (L.build_load (L.build_struct_gep g2 0 "tmp" builder) "val" builder) (A.TList ntyp) builder in
        let (edgescomp, builder) = compare (L.build_load (L.build_struct_gep g1 1 "tmp" builder) "val" builder)
                                        (L.build_load (L.build_struct_gep g2 1 "tmp" builder) "val" builder)  (A.TList etyp) builder in
        let (relcomp, builder) = compare (L.build_load (L.build_struct_gep g1 2 "tmp" builder) "val" builder)
                                          (L.build_load (L.build_struct_gep g2 2 "tmp" builder) "val" builder) ereltyp builder in
        (L.build_mul nodescomp (L.build_mul edgescomp relcomp "tmp" builder) "tmp" builder, builder)
    | A.TList(_) -> compare_list e1 e2 t builder
    )

  and compare_fields n fields rec1 rec2 builder = 
  (match fields with 
    [] -> (L.const_int i1_t 1, builder) (*empty recs are equal*)
  | (_,t)::tl -> let (cmp, builder) = compare (L.build_load (L.build_struct_gep rec1 n "tmp" builder) "val" builder) 
                                              (L.build_load (L.build_struct_gep rec2 n "tmp" builder) "val" builder) t builder in
                  let (restcmp, builder) = compare_fields (n+1) tl rec1 rec2 builder in
                  (L.build_mul cmp restcmp "tmp" builder, builder)
  )
  

    and compare_list lst1 lst2 t builder =  
      let list_typ = get_list_type t in
      let struct1 = L.build_alloca (ltype_of_typ t) "strct" builder in ignore(L.build_store lst1 struct1 builder);
      let struct2 = L.build_alloca (ltype_of_typ t) "strct" builder in ignore(L.build_store lst2 struct2 builder);

      let len1 = L.build_load (L.build_struct_gep struct1 1 "tmp" builder) "len" builder
      and len2 = L.build_load (L.build_struct_gep struct2 1 "tmp" builder) "len" builder in
     
      let comp_val = L.build_icmp L.Icmp.Eq len1 len2 "tmp" builder in
      let merge_bb = L.append_block context "merge" the_function in

      let then_bb = L.append_block context "compare" the_function in 

      ignore (L.build_cond_br comp_val then_bb merge_bb builder);

      (*copy over old list elements by effectively using a for-in loop*) 
      let then_builder = L.builder_at_end context then_bb in
      let lstvals1 = L.build_load (L.build_struct_gep struct1 0 "tmp" then_builder) "lst" then_builder and
      lstvals2 = L.build_load (L.build_struct_gep struct2 0 "tmp" then_builder) "lst" then_builder  in
      let elind = L.build_alloca i32_t "ind" then_builder in ignore(L.build_store (L.const_int i32_t 0) elind then_builder);

      let pred_bb = L.append_block context "checklimits" the_function in
      ignore (L.build_br pred_bb then_builder); 

      let body_bb = L.append_block context "comparison" the_function in
      let body_builder = L.builder_at_end context body_bb in
      let ind = L.build_load elind "i" body_builder in
      let p1 = L.build_in_bounds_gep lstvals1 [|ind|] "ptr" body_builder and p2 = L.build_in_bounds_gep lstvals2 [|ind|] "ptr" body_builder in
      let el1 = (L.build_load p1 "tmp" body_builder) and el2 = (L.build_load p1 "tmp" body_builder) in 
      let (elcomp, body_builder) = compare el1 el2 list_typ body_builder in
      let comp_val = L.build_mul comp_val elcomp "tmp" body_builder in
      
      ignore(L.build_store (L.build_add (L.build_load elind "tmp" body_builder) (L.const_int i32_t 1) "inc" body_builder) elind body_builder);
      add_terminal body_builder (L.build_br pred_bb);

      let pred_builder = L.builder_at_end context pred_bb in
      let bool_val = L.build_icmp L.Icmp.Slt (L.build_load elind "tmp" pred_builder) len1 "comp" pred_builder in

      ignore (L.build_cond_br bool_val body_bb merge_bb pred_builder);

      let end_builder = L.builder_at_end context merge_bb in
      (comp_val, end_builder)
  in
  let rec assign_array ar els n builder = (*stores elements, starting with element n in ar, returns ar*)
    match els with
      [] -> ar
    | e::tl -> let p = L.build_in_bounds_gep ar [|(L.const_int i32_t n)|] "ptr" builder in
                      ignore(L.build_store e p builder); assign_array ar tl (n+1) builder
    in

    let add_to_list lst el t builder = 
      let list_typ = get_list_type t and newstruct = L.build_alloca (ltype_of_typ t) "strct" builder in
      let oldstruct = L.build_alloca (ltype_of_typ t) "strct" builder in ignore(L.build_store lst oldstruct builder);

      let oldlen = L.build_load (L.build_struct_gep oldstruct 1 "tmp" builder) "len" builder
      and oldlst = L.build_load (L.build_struct_gep oldstruct 0 "tmp" builder) "lst" builder in
      let newlen = L.build_add (L.const_int i32_t 1) oldlen "len" builder in
      ignore(L.build_store newlen (L.build_struct_gep newstruct 1 "tmp" builder) builder);

      let newlst = L.build_array_alloca(ltype_of_typ list_typ) newlen "lst" builder  in
      ignore(L.build_store el (L.build_in_bounds_gep newlst [|oldlen|] "ptr" builder) builder);

      let elind = L.build_alloca i32_t "ind" builder in ignore(L.build_store (L.const_int i32_t 0) elind builder);


      (*copy over old list elements by effectively using a for-in loop*) 
      let pred_bb = L.append_block context "checklimits" the_function in
      ignore (L.build_br pred_bb builder); 

      let body_bb = L.append_block context "assignment" the_function in
      let body_builder = L.builder_at_end context body_bb in
      let ind = (L.build_load elind) "i" body_builder in
      let oldp = L.build_in_bounds_gep oldlst [|ind|] "ptr" body_builder and newp = L.build_in_bounds_gep newlst [|ind|] "ptr" body_builder 
      in ignore(L.build_store (L.build_load oldp "tmp" body_builder) newp body_builder);
      
      ignore(L.build_store (L.build_add (L.build_load elind "tmp" body_builder) (L.const_int i32_t 1) "inc" body_builder) elind body_builder);
      add_terminal body_builder (L.build_br pred_bb);

      let pred_builder = L.builder_at_end context pred_bb in
      let bool_val = L.build_icmp L.Icmp.Slt (L.build_load elind "tmp" pred_builder) oldlen "comp" pred_builder in

      let merge_bb = L.append_block context "merge" the_function in
      ignore (L.build_cond_br bool_val body_bb merge_bb pred_builder);

      let end_builder = L.builder_at_end context merge_bb in
      ignore(L.build_store newlst (L.build_struct_gep newstruct 0 "tmp" end_builder) end_builder);
      (L.build_load newstruct "strct" end_builder, end_builder)
    
    in
    
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

    let list_ops e1 e2 t op builder =
    (match op with
      A.Ladd -> add_to_list e1 e2 t builder
    | _ -> raise(Failure "wrong operation applied to lists")
    )

  in

    let graph_ops e1 e2 t op builder =
      let (ntyp, etyp) = get_graph_types t in
      let gstruct = L.build_alloca (ltype_of_typ t) "strct" builder in ignore(L.build_store e1 gstruct builder);
      (match op with
        A.Gadd -> let oldns = L.build_load (L.build_struct_gep gstruct 0 "ptr" builder) "nodes" builder in
                  let (newns, builder) =  add_to_list oldns e2 (A.TList ntyp) builder in
                  ignore(L.build_store newns (L.build_struct_gep gstruct 0 "tmp" builder) builder);
                  (gstruct, builder)
      | A.Eadd -> let oldes = L.build_load (L.build_struct_gep gstruct 1 "ptr" builder) "nodes" builder in
                  let (newes, builder) =  add_to_list oldes e2 (A.TList etyp) builder in
                  ignore(L.build_store newes (L.build_struct_gep gstruct 1 "tmp" builder) builder);
                  (gstruct, builder)
      | _ -> raise(Failure "wrong operation applied to graphs")
      )

in

  let rec copy_list lst t builder = 
    let list_typ = get_list_type t and newstruct = L.build_alloca (ltype_of_typ t) "strct" builder in
    let oldstruct = L.build_alloca (ltype_of_typ t) "strct" builder in ignore(L.build_store lst oldstruct builder);

    let len = L.build_load (L.build_struct_gep oldstruct 1 "tmp" builder) "len" builder
    and oldlst = L.build_load (L.build_struct_gep oldstruct 0 "tmp" builder) "lst" builder in
    ignore(L.build_store len (L.build_struct_gep newstruct 1 "tmp" builder) builder);

    let newlst = L.build_array_alloca(ltype_of_typ list_typ) len "lst" builder  in
    let elind = L.build_alloca i32_t "ind" builder in ignore(L.build_store (L.const_int i32_t 0) elind builder);


    (*copy over old list elements by effectively using a for-in loop*) 
    let pred_bb = L.append_block context "checklimits" the_function in
    ignore (L.build_br pred_bb builder); 

    let body_bb = L.append_block context "assignment" the_function in
    let body_builder = L.builder_at_end context body_bb in
    let ind = L.build_load elind "i" body_builder in
    let oldp = L.build_in_bounds_gep oldlst [|ind|] "ptr" body_builder and newp = L.build_in_bounds_gep newlst [|ind|] "ptr" body_builder in
    let oldel = (L.build_load oldp "tmp" body_builder) in let (newel, body_builder) = copy oldel list_typ body_builder in
    ignore(L.build_store newel newp body_builder);
    
    ignore(L.build_store (L.build_add (L.build_load elind "tmp" body_builder) (L.const_int i32_t 1) "inc" body_builder) elind body_builder);
    add_terminal body_builder (L.build_br pred_bb);

    let pred_builder = L.builder_at_end context pred_bb in
    let bool_val = L.build_icmp L.Icmp.Slt (L.build_load elind "tmp" pred_builder) len "comp" pred_builder in

    let merge_bb = L.append_block context "merge" the_function in
    ignore (L.build_cond_br bool_val body_bb merge_bb pred_builder);

    let end_builder = L.builder_at_end context merge_bb in
    ignore(L.build_store newlst (L.build_struct_gep newstruct 0 "tmp" end_builder) end_builder);
    (L.build_load newstruct "strct" end_builder, end_builder)

and copy e t builder = (*returns a deep copy of e and the builder at the end of copy*)
  (match t with
      A.TInt -> (e, builder) (*no need to deep copy for primitive types*)
    | A.TChar -> (e, builder)
    | A.TBool -> (e, builder)
    | A.TVoid -> (e, builder) 
    | A.TString -> (e, builder)
    | A.TFloat -> (e, builder)
    | A.TList _ -> copy_list e t builder
    | A.TRec(_, fields) -> let newrec = L.build_alloca (ltype_of_typ t) "strct" builder in 
        let oldrec = L.build_alloca (ltype_of_typ t) "strct" builder in ignore(L.build_store e oldrec builder);
        copy_fields 0 fields newrec oldrec builder
    | A.TEdge(_, trec1, trec2) -> let newe = L.build_alloca (ltype_of_typ t) "edge" builder in
        let olde = L.build_alloca (ltype_of_typ t) "edge" builder in ignore(L.build_store e olde builder);
        let (newfrom, builder) = copy (L.build_load (L.build_struct_gep olde 0 "tmp" builder) "val" builder) trec1 builder in
        let (newto, builder) = copy (L.build_load (L.build_struct_gep olde 1 "tmp" builder) "val" builder) trec1 builder in
        let (newdir, builder) = copy (L.build_load (L.build_struct_gep olde 2 "tmp" builder) "val" builder) A.TBool builder in
        let (newrel, builder) = copy (L.build_load (L.build_struct_gep olde 3 "tmp" builder) "val" builder) trec2 builder in
        ignore(L.build_store newfrom (L.build_struct_gep newe 0 "tmp" builder) builder);
        ignore(L.build_store newto (L.build_struct_gep newe 1 "tmp" builder) builder);
        ignore(L.build_store newdir (L.build_struct_gep newe 2 "tmp" builder) builder);
        ignore(L.build_store newrel (L.build_struct_gep newe 3 "tmp" builder) builder);
        (L.build_load newe "edge" builder, builder)
    | A.TGraph(_, ntyp, etyp) -> let ereltyp = (match etyp with A.TEdge(_, _, rel) -> rel | _ -> raise(Failure "wrong edge type")) in
        let newg = L.build_alloca (ltype_of_typ t) "graph" builder in
        let oldg = L.build_alloca (ltype_of_typ t) "graph" builder in ignore(L.build_store e oldg builder);
        let (newnodes, builder) = copy (L.build_load (L.build_struct_gep oldg 0 "tmp" builder) "val" builder) (A.TList ntyp) builder in
        let (newedges, builder) = copy (L.build_load (L.build_struct_gep oldg 1 "tmp" builder) "val" builder) (A.TList etyp) builder in
        let (newrel, builder) = copy (L.build_load (L.build_struct_gep oldg 2 "tmp" builder) "val" builder) ereltyp builder in
        ignore(L.build_store newnodes (L.build_struct_gep newg 0 "tmp" builder) builder);
        ignore(L.build_store newedges (L.build_struct_gep newg 1 "tmp" builder) builder);
        ignore(L.build_store newrel (L.build_struct_gep newg 2 "tmp" builder) builder);
        (L.build_load newg "graph" builder, builder)
    | _ -> raise(Failure "not a valid type")

  )

and copy_fields n fields newrec oldrec builder = 
  (match fields with 
    [] -> (L.build_load newrec "rec" builder, builder)
  | (_,t)::tl -> let (newval, builder) = copy (L.build_load (L.build_struct_gep oldrec n "tmp" builder) "val" builder) t builder in
                  ignore(L.build_store newval (L.build_struct_gep newrec n "tmp" builder) builder); copy_fields (n+1) tl newrec oldrec builder
  )

in

let get_expr_type e = 
  (match e with
    A.AIntLit(_, t) -> t
  | A.ACharLit(_, t) -> t
  | A.ABoolLit(_, t) -> t
  | A.AStrLit(_, t) -> t
  | A.AFloatLit(_, t) -> t
  | A.AId(_, t) -> t
  | A.ABinop(_, _, _, t) -> t 
  | A.AUnop(_, _, t) -> t
  | A.ACall(_, _, _, _, t) -> t
  | A.AList(_, t) -> t
  | A.AItem(_, _, t) -> t
  | A.ARecord(_, t) -> t
  | A.ADot(_, _, t) -> t
  | A.AEdge(_, _, _, _, t) -> t
  | A.AGraph(_, _, t) -> t
  | A.ANoexpr(t) -> t)

in

  let rec build_expressions l builder local_var_map = (*builds all aexprs in l, updating builder appropriately: basically combine map and fold*)
    (match l with
      [] -> ([], builder)
    | e::tl -> let (exp, newbuilder) = aexpr builder local_var_map e in 
            let (other_exps, endbuilder) = 
            build_expressions tl newbuilder local_var_map in (exp::other_exps, endbuilder))

  and build_list_from_els l t builder local_var_map = 
    let list_typ = get_list_type t and (els, newbuilder) = build_expressions l builder local_var_map in 
    let struct_var = L.build_alloca (ltype_of_typ t) "strct" newbuilder in
    let ar_var = L.build_array_alloca (ltype_of_typ list_typ) (L.const_int i32_t (List.length l)) "lst" newbuilder in
    let init_list = assign_array ar_var els 0 newbuilder in
    let p0 = L.build_struct_gep struct_var 0 "p0" newbuilder and p1 = L.build_struct_gep struct_var 1 "p1" newbuilder in
    ignore(L.build_store init_list p0 newbuilder); ignore(L.build_store (L.const_int i32_t (List.length l)) p1 newbuilder);
    (L.build_load struct_var "lst" newbuilder, newbuilder) 

  and build_edge_with_record e1 op e2 erec typ builder local_var_map = 
           let (directed,from,into) =
            match op with
             | A.Dash -> (false,e1,e2)
             | A.To -> (true,e1,e2)
             | A.From -> (true,e2,e1)
             | _ -> raise(Failure("undefined edge type"))
             
             in let get_ptr e = 
                  (match e with 
                    A.AId(n,_) -> 
                        (try StringMap.find n local_var_map
                        with Not_found -> 
                        raise (Failure ("undeclared variable " ^ n)))
                       
                    | _-> raise (Failure ("Not supported.Node must be declared"))
                   )
            (*in let (erec, builder) = aexpr builder local_var_map item*)

            in let argslist =
            [   get_ptr from;
                get_ptr into;
                fst (aexpr builder local_var_map (A.ABoolLit(directed,A.TBool)));
                erec
            ]

         in let loc = L.build_alloca (ltype_of_typ typ) "" builder
     in let rec populate_structure fields i = 
      match fields with 
      | [] -> L.build_load loc "" builder
      | hd :: tl ->
            ( let eptr = L.build_struct_gep loc i "ptr" builder
                in ignore(L.build_store hd eptr builder);
          populate_structure tl (i+1) 
              )
    in (populate_structure argslist 0, builder)


  and aexpr builder local_var_map = function
        A.AIntLit(i, _) -> (L.const_int i32_t i, builder)
      | A.ABoolLit(b, _) -> (L.const_int i1_t (if b then 1 else 0), builder)
      | A.AStrLit(s, _) -> (L.build_global_stringptr s "str" builder, builder)
      | A.ACharLit(c, _) -> (L.const_int i8_t (C.code c), builder)
      | A.AFloatLit(f, _) -> (L.const_float float_t f, builder)
      | A.AId(s,_) -> (L.build_load (lookup s local_var_map) s builder, builder)
      | A.AList(l, t) ->  build_list_from_els l t builder local_var_map
      | A.AItem(s, e, _) -> let strct = lookup s local_var_map in let arp = L.build_struct_gep strct 0 "tmp" builder in
                            let ar = L.build_load arp "tmpar" builder and (ad, builder) = aexpr builder local_var_map e in
                            let p = L.build_in_bounds_gep ar [|ad|] "ptr" builder in (L.build_load p "item" builder, builder)
      | A.ACall ("print", [e], _, _, _) -> let (e', builder') =  (aexpr builder local_var_map e) in 
                                      (L.build_call printf_func [| e' |] "printf" builder', builder')
      | A.ACall ("sample_display", [e], _, _, _) -> let (e', builder') =  (aexpr builder local_var_map e) in 
                                      (L.build_call display_func [| e' |] "sample_display" builder', builder')
      | A.ACall("printint", [e], _, _, _) | A.ACall ("printbool", [e], _, _, _) -> let (e', builder') =  (aexpr builder local_var_map e) in
                                        (L.build_call printf_func [| int_format_str ; e' |] "printf" builder', builder')
      | A.ACall("printfloat", [e], _, _, _) -> let (e', builder') =  (aexpr builder local_var_map e) in 
                                (L.build_call printf_func [| float_format_str ; e' |] "printf" builder', builder')
      |A.ACall("size", [e], _, _, _) -> let (e', builder') = (aexpr builder local_var_map e) in 
        let strct = L.build_alloca (L.type_of e') "strct" builder' in ignore(L.build_store e' strct builder');
        (L.build_load (L.build_struct_gep strct 1 "tmp" builder') "len" builder', builder')
      | A.ACall (_, act, _, callname, _) ->
        let (fdef, fdecl) = StringMap.find callname function_decls in
        let (actuals', builder') = build_expressions (List.rev act) builder local_var_map in
        let actuals = List.rev actuals' in
        let result = (match fdecl.A.typ with A.TVoid -> ""
                                           | _ -> callname ^ "_result") in
        (L.build_call fdef (Array.of_list actuals) result builder', builder')
      | A.AUnop(op, e, _) ->
            let (e', builder') = aexpr builder local_var_map e in
           ((match op with
            A.Neg     -> L.build_neg
           | A.Not     -> L.build_not) e' "tmp" builder', builder')
      | A.ABinop (e1, op, e2, t) ->     let (e1', builder1) = aexpr builder local_var_map e1
        in let (e2', builder') = (
          match e2 with
          A.AEdge(n1, op, n2, rel, typ) -> 
            (match rel with
              A.ANoexpr _ -> let g = L.build_alloca (ltype_of_typ t) "g" builder1 in ignore(L.build_store e1' g builder1);
                          build_edge_with_record n1 op n2 (L.build_load (L.build_struct_gep g 2 "ptr" builder1) "tmp" builder1) typ builder1 local_var_map
            | _ -> aexpr builder1 local_var_map e2
            )
        |  _  -> aexpr builder1 local_var_map e2 )
      in
      let et = get_expr_type e1 in
      (match op with
      A.Equal -> compare e1' e2' et builder
    | A.Neq -> let (compval, builder) = compare e1' e2' et builder in (L.build_sub (L.const_int i1_t 1) compval "tmp" builder, builder)
    | _ ->  (match et with 
         | A.TFloat -> ((float_ops op) e1' e2' "tmp" builder', builder')
         | A.TList _ -> list_ops e1' e2' t op builder'
         | A.TGraph(_,_,_) -> graph_ops e1' e2' t op builder'
         | _ -> ((int_ops op) e1' e2' "tmp" builder', builder')                                            
        ))
        | A.ANoexpr _ -> (L.const_int i32_t 0, builder)
      | A.ARecord(alist,trec) ->
            let (argslist, builder) = build_expressions (List.map (fun f -> (snd f)) alist) builder local_var_map
         in let loc = L.build_alloca (ltype_of_typ trec) "" builder
		 in let rec populate_structure fields i = 
			match fields with 
			| [] -> L.build_load loc "" builder
			| hd :: tl ->
	          ( let eptr = L.build_struct_gep loc i "ptr" builder
                in ignore(L.build_store hd eptr builder);
			    populate_structure tl (i+1) 
              )
		in (populate_structure argslist 0, builder)
       | A.AEdge(e1,op,e2,item,typ) -> let (rel, builder) = aexpr builder local_var_map item in
          build_edge_with_record e1 op e2 rel typ builder local_var_map 
      | A.ADot(e1,entry,_) ->
           let rec match_name lst n = 
             match lst with 
                | [] -> raise (Failure ("Not found"))
                | h :: t -> if h = n then 0 else 
                            1 + match_name t n
          in
          let trec = get_expr_type e1 in
          let alist = (match trec with 
            A.TRec(_, l) -> l
          | _ -> raise(Failure ("dot applied to wrong type"))) in

          let mems = List.map fst alist in
          (match e1 with
            | A.AId(name,_) ->  
                let index = match_name mems entry
                in let load_loc = lookup name local_var_map 
                in let ext_val = L.build_struct_gep load_loc index "ext_val" builder      
                in (L.build_load ext_val "" builder, builder)
            |_ ->
                let (e',builder) = aexpr builder local_var_map e1
                in 
                let loc = L.build_alloca (L.type_of e') "e" builder in
                let _ = L.build_store e' loc builder
                in 
                let mems = 
			         List.map (fun (id,_)  -> id) alist 
                       
                in let index = match_name mems entry
                in let ext_val = L.build_struct_gep loc index "ext_val" builder      
                in (L.build_load ext_val "" builder, builder))
           
        | A.AGraph(lst, rel, t) -> 
          let rec split_lists l = 
            match l with
              [] -> ([], [], [])
              | h::tl -> let (nodes, edges, ids) = split_lists tl in
              let typ = get_expr_type h in 
              (match typ with
                A.TRec(_, _) -> (match h with
                                A.AId(name, _) -> if List.mem name ids then (nodes, edges, ids) 
                                                else (h::nodes, edges, name::ids)
                                |            _ -> (h::nodes, edges, ids))
              | A.TEdge(_, _, _) -> (match h with
                                    A.AEdge(node1, o, node2, r, ty) ->
                                      let (newnodes, newids) =
                                        (match node1 with
                                          A.AId(name, _) -> if List.mem name ids then (nodes, ids) else (node1::nodes, name::ids)
                                        | _ -> (nodes, ids)
                                        )
                                      in let (newnodes, newids) = 
                                        (match node2 with
                                          A.AId(name, _) -> if List.mem name newids then (newnodes, newids) else (node1::newnodes, name::newids)
                                        | _ -> (newnodes, newids)
                                        ) 
                                      in let newe =
                                        (match r with
                                          A.ANoexpr(_) -> A.AEdge(node1, o, node2, rel, ty)
                                        | _ -> h)
                                      in (newnodes, newe::edges, newids)               

                                    | _ -> (nodes, h::edges, ids))
              | _ -> raise(Failure "wrong type given to graph constructor")
                            )
          in

          let (nodes, edges, _) = split_lists lst in 
          let (grel, builder) = aexpr builder local_var_map rel in
          let graph = L.build_alloca (ltype_of_typ t) "g" builder in
          ignore(L.build_store grel (L.build_struct_gep graph 2 "ptr" builder) builder);

          let (ntyp, etyp) = get_graph_types t in
          let (nlist, builder) = build_list_from_els nodes (A.TList ntyp) builder local_var_map in
          let (elist, builder) = build_list_from_els edges (A.TList etyp) builder local_var_map in
          ignore(L.build_store nlist (L.build_struct_gep graph 0 "ptr" builder) builder);
          ignore(L.build_store elist (L.build_struct_gep graph 1 "ptr" builder) builder);
          (L.build_load graph "g" builder, builder)



            
    (* Build the code for the given statement; return the builder for
       the statement's successor *)

    in let rec astmt (builder, local_var_map) = function
          A.AExpr(e) -> ((snd (aexpr builder local_var_map e)), local_var_map)
        | A.AReturn(e, t) -> (match t with
              A.TVoid -> ignore(L.build_ret_void builder); (builder, local_var_map)
            | _ -> let (e', builder') = (aexpr builder local_var_map e) in ignore(L.build_ret e' builder'); (builder', local_var_map))
        | A.AAsn(s, e, b, t) -> 
          let (e', builder') = aexpr builder local_var_map e in
          let (e', builder') = if b then (e', builder') else copy e' t builder' in
          let add_local m (t,n) =            
            let local_var = L.build_alloca (ltype_of_typ t) n builder'
            in StringMap.add n local_var m in

          (match s with
            A.AId(name, _) -> 
            let local_var_map = if StringMap.mem name local_var_map 
            then local_var_map 
            else add_local local_var_map (t,name) in 
            ignore (L.build_store e' (lookup name local_var_map) builder'); (builder', local_var_map)
            | A.AItem(name, adr, _) ->
            let arp = L.build_struct_gep (lookup name local_var_map) 0 "tmp" builder and (ad, builder') = aexpr builder' local_var_map adr in
            let ar = L.build_load arp "tmpar" builder' in
            let p = L.build_in_bounds_gep ar [|ad|] "ptr" builder' in ignore(L.build_store e' p builder'); (builder', local_var_map)
          | A.ADot(r, entry, _) ->
            let rec match_name lst n = 
              (match lst with 
                [] -> raise (Failure ("Not found"))
                | h :: t -> if h = n then 0 else 
                            1 + match_name t n)
            in
            let name = 
              (match r with 
                A.AId(s, _) -> s
              | _ -> raise(Failure("invalid lvalue"))
              )
            in
            let rtype = get_expr_type r in
            let alist = (match rtype with
               A.TRec(_, l) -> l
              | _ -> raise( Failure "wrong type provided to record") )
                in
            let mems = List.map fst alist in
            let index = match_name mems entry in
            let recval = (lookup name local_var_map) in
            let ptr = L.build_struct_gep recval index "ptr" builder' in
            ignore(L.build_store e' ptr builder'); (builder', local_var_map)
            | _ -> raise(Failure "invalid lvalue"))


                 
        | A.AIf (predicate, then_stmt, else_stmt) ->
          let (bool_val, builder) = aexpr builder local_var_map predicate in
          let merge_bb = L.append_block context "merge" the_function in

          let then_bb = L.append_block context "then" the_function in
          add_terminal (fst (List.fold_left astmt ((L.builder_at_end context then_bb), local_var_map) then_stmt))
            (L.build_br merge_bb);

          let else_bb = L.append_block context "else" the_function in
          add_terminal (fst (List.fold_left astmt ((L.builder_at_end context else_bb), local_var_map) else_stmt))
            (L.build_br merge_bb);

          ignore (L.build_cond_br bool_val then_bb else_bb builder);
          (L.builder_at_end context merge_bb, local_var_map)

        | A.AWhile (predicate, body) ->
          let pred_bb = L.append_block context "while" the_function in
          ignore (L.build_br pred_bb builder);

          let body_bb = L.append_block context "while_body" the_function in
          add_terminal (fst (List.fold_left astmt ((L.builder_at_end context body_bb), local_var_map) body))
            (L.build_br pred_bb);

          let pred_builder = L.builder_at_end context pred_bb in
          let (bool_val, pred_builder) = aexpr pred_builder local_var_map predicate in

          let merge_bb = L.append_block context "merge" the_function in
          ignore (L.build_cond_br bool_val body_bb merge_bb pred_builder);
          (L.builder_at_end context merge_bb, local_var_map)


        | A.AFor (s1, e2, s3, body) -> List.fold_left astmt (builder, local_var_map)
                                         [s1 ; A.AWhile(e2, List.rev (s3::(List.rev body)))]
                                         (* Build the code for each statement in the function *)

        | A.AForin (e1, e2, body) -> 
        let ind = (match e1 with A.AId(s, t) -> (s, t) | _ -> raise(Failure "invalid for loop")) in let lst = L.build_alloca (ltype_of_typ (A.TList (snd ind))) "lst" builder in
        let (e2', builder) = (aexpr builder local_var_map e2)
        in ignore(L.build_store e2' lst builder);
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
        add_terminal endbody_builder (L.build_br pred_bb);

        let pred_builder = L.builder_at_end context pred_bb in
        let bool_val = L.build_icmp L.Icmp.Slt (L.build_load elind "tmp" pred_builder) endlst "comp" pred_builder in

        let merge_bb = L.append_block context "merge" the_function in
        ignore (L.build_cond_br bool_val body_bb merge_bb pred_builder);
        (L.builder_at_end context merge_bb, new_local_var_map)

          


    in let (builder,_) = List.fold_left 
           astmt (builder,local_vars) afunc.A.body 
    in
    (* Add a return if the last block falls off the end *)
    add_terminal builder (match afunc.A.typ with
          A.TVoid -> L.build_ret_void
          | t -> L.build_ret (L.const_int (ltype_of_typ t) 0) 
        )
    in


  List.iter build_function_body functions;
  the_module



