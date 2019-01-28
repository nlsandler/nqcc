open Batteries

let generate filename prog =

  (* Open assembly file for writing *)
  let filename_asm = String.splice filename (-1) 1 "s" in
  let chan = open_out filename_asm in
  (* TODO: add newline! *)
  let print_asm = Printf.fprintf chan "%s\n" in

  let handle_error message = begin
      Printf.printf "ERROR: %s\n" message;
      close_out chan;
      Sys.remove filename_asm;
      exit 1;
    end in

  let var_lookup = Context.var_lookup handle_error in

  let validate_fun_call context fun_name arg_count =
    match var_lookup context fun_name with
    | Context.Fun i ->
       if i <> arg_count then
         handle_error "called function with wrong number of arguments"
       else ()
    | _ -> handle_error "tried to call identifier that isn't a function"
  in

  let emit_label label = Printf.fprintf chan "%s:\n" label in

  let emit_comparison set_instruction =
    begin
      print_asm "    cmp %ecx, %eax";
      print_asm "    movl $0, %eax";
      Printf.fprintf chan "    %s %%al\n" set_instruction
    end in

  let emit_function_epilogue () =
    begin
      print_asm "    movl %ebp, %esp";
      print_asm "    pop %ebp";
      print_asm "    ret"
    end in

  let emit_bin_op op =
    let open Ast in
    match op with
    | Div ->
       (* zero out edx (b/c idivl operand calculates 64-bit value edx:eax / operand) *)
       begin
         print_asm "    xor %edx, %edx";
         print_asm "    idivl %ecx";
       end
    | Mod ->
       (* zero out edx (b/c idivl operand calculates 64-bit value edx:eax / operand) *)
       begin
         print_asm "    xor %edx, %edx";
         print_asm "    idivl %ecx";
         (* remainder stored in edx, move it to eax *)
         print_asm "    movl %edx, %eax"
       end
    | Sub -> print_asm "    subl %ecx, %eax"
    | Add -> print_asm "    addl %ecx, %eax";
    | Mult -> print_asm "    imul %ecx, %eax";
    | Xor -> print_asm "    xor %ecx, %eax";
    | BitAnd -> print_asm "    and %ecx, %eax";
    | BitOr -> print_asm "    or %ecx, %eax";
    | ShiftL -> print_asm "    sall %cl, %eax";
    | ShiftR -> print_asm "    sarl %cl, %eax";
    | Eq -> emit_comparison "sete"
    | Neq -> emit_comparison "setne"
    | Lt -> emit_comparison "setl"
    | Le -> emit_comparison "setle"
    | Gt -> emit_comparison "setg"
    | Ge -> emit_comparison "setge"
    | Or ->
       begin
         print_asm "    orl %eax, %ecx";
         print_asm "    movl $0, %eax";
         print_asm "    setne %al"
       end
    | And ->
       begin
         (* if eax != 0, set al = 1 *)
         print_asm "    cmp $0, %eax";
         print_asm "    movl $0, %eax"; (* zero this b/c we'll store result in it*)
         print_asm "    setne %al";
         (* if ecx != 0, set cl = 1 *)
         print_asm "    cmp $0, %ecx";
         print_asm "    setne %cl";
         (* eax = al && cl *)
         print_asm "    andb %cl, %al"
       end in

  (* TODO refactor a lot of these print functions, e.g. align *)
  let emit_local_heap_decl lbl = function
    | Context.(HeapDecl (_, Final init)) ->
       if init = 0 then
         begin
           print_asm "    .text";
           Printf.fprintf chan "    .zerofill __DATA,__bss,_%s,4,2\n" lbl
         end
       else
         begin
           print_asm "    .data";
           print_asm "    .align 2";
           Printf.fprintf chan "_%s:\n" lbl;
        Printf.fprintf chan "    .long %d\n" init
         end
    | other -> failwith "tentative definition of local var"
  in

  let emit_local_heap_decls = Map.iter emit_local_heap_decl in

  let print_globl_if_extern linkg lbl =
    if linkg = Context.External
    then Printf.fprintf chan "    .globl _%s\n" lbl
    else ()
  in

  let emit_global_heap_decl lbl = function
    | Context.HeapDecl (linkg, init) ->
        let open Context in
        let init' = finalize_init init in
        begin
          match init' with
          | NoDef -> ()
          | Final 0 ->
             if linkg = External then
               (* allocate space in .comm - NOTE different on Linux! *)
               begin
                 print_asm "    .text";
                 Printf.fprintf chan "    .comm _%s,4,2\n" lbl
               end
             else
               (* allocate space in bss *)
               begin
                 print_asm "    .text";
                 Printf.fprintf chan "    .zerofill __DATA,__bss,_%s,4,2\n" lbl
               end
          | Final i ->
             begin
               print_asm "    .text";
               print_globl_if_extern linkg lbl;
               print_asm "    .data";
               print_asm "    .align 2";
               Printf.fprintf chan "_%s:\n" lbl;
               Printf.fprintf chan "    .long %d\n" i
             end
          | Tentative -> failwith "failed to finalize tentative def"
        end
    | _ -> () (* function declaration - already handled *)
  in

  let emit_global_heap_decls = Map.iter emit_global_heap_decl in

  (* generate code to execute expression and move result into eax *)
  let rec generate_exp context = function
    | Ast.(Assign (Equals, (ID id), exp)) ->
       let _ = generate_exp context exp in
       (* get location of variable in memory *)
       begin
         match var_lookup context id with
         | Context.Stack var_index -> Printf.fprintf chan "    movl %%eax, %d(%%ebp)\n" var_index
         | Context.Heap lbl -> Printf.fprintf chan "    movl %%eax, _%s\n" lbl
         | Context.Fun _ -> handle_error "tried to assign function to variable"
       end

    | TernOp (e1, e2, e3) ->
       let post_tern_label = Util.unique_id "post_ternary" in
       let e3_label = Util.unique_id "second_branch_label" in
       begin
         (* calculate cond *)
         generate_exp context e1;
         (* is cond true? *)
         print_asm "    cmp   $0, %eax";
         (* if it's false, jump to 'else' exp *)
         Printf.fprintf chan "    je   %s\n" e3_label;
         (* Generate 'if' block *)
         generate_exp context e2;
         (* after 'if', jump over 'else' *)
         Printf.fprintf chan "    jmp    %s\n" post_tern_label;
         (* Label start of 'else' *)
         emit_label e3_label;
         (* Generate 'else' *)
         generate_exp context e3;
         (* Label end of ternary operation *)
         emit_label post_tern_label
       end
    | BinOp (op, e1, e2) ->
       begin
         (* calculate e1 and e2 *)
         generate_exp context e1;
         print_asm "    push %eax";
         generate_exp context e2;
         (* op s, d computes d = op(d,s), so put e2 in ecx, e1 in eax *)
         print_asm "    movl %eax, %ecx";
         (* Put e1 in eax (where it needs to be for idiv) *)
         print_asm  "    pop %eax";
         (* perform operation *)
         emit_bin_op op
       end
    | UnOp (op, e) ->
       generate_exp context e;
       begin
         match op with
         | Pos -> ()(* No-op for now - eventually handle casting to int if needed *)
         | Negate -> print_asm "    neg %eax";
         | Complement -> print_asm "    not %eax";
         | Not ->
            print_asm "    cmpl $0, %eax";     (* compare eax to 0 *)
            print_asm "    movl $0, %eax";     (* set eax to 0 *)
            print_asm "    sete %al";         (* if eax was zero in earlier comparison, set al to 1 *)
       end
    | Var (ID id) ->
       begin
         (* move value  of variable to eax *)
         match var_lookup context id with
         | Context.Stack var_index -> Printf.fprintf chan "    movl %d(%%ebp), %%eax\n" var_index
         | Context.Heap lbl -> Printf.fprintf chan "    movl _%s, %%eax\n" lbl
         (* NOTE: this does not actually violate the spec *)
         | Context.Fun _ -> handle_error "Trying to reference function as variable"
       end
    | FunCall (ID id, args) ->
       let arg_count = List.length args in
       let _ = validate_fun_call context id arg_count in
       let _ =
         (* edx = (esp - 4*(arg_count + 1)) % 16 *)
         (* the + 1 is for saved remainder *)
         print_asm "    movl %esp, %eax";
         Printf.fprintf chan "    subl $%d, %%eax\n" (4*(arg_count + 1));
         print_asm "    xorl %edx, %edx";
         print_asm "    movl $0x20, %ecx";
         print_asm "    idivl %ecx";
         (* edx contains the remainder, i.e. # of bytes to subtract *)
         print_asm "    subl %edx, %esp";
         print_asm "    pushl %edx" (* need it for deallocating *)
       in
       let _ = put_args_on_stack context args in
       begin
         (* actually make the call *)
         Printf.fprintf chan "    call _%s\n" id;
         (* deallocate args *)
         Printf.fprintf chan "    addl $%d, %%esp\n" (arg_count * 4);
         (* pop remainder off stack, undo 16-byte alignment *)
         print_asm "    popl %edx";
         print_asm "    addl %edx, %esp"
       end
    | Const (Int i) ->
      Printf.fprintf chan "    movl    $%d, %%eax\n" i
    | Const (Char c) ->
       Printf.fprintf chan "    movl    $%d, %%eax\n" (Char.code c)
    | _ -> handle_error "Constant not supported"

  and put_args_on_stack context args =
    let push_arg arg =
      generate_exp context arg;
      print_asm "    pushl %eax";
    in
    List.iter push_arg (List.rev args)
  in

  let generate_optional_exp context = function
    | None -> ()
    | Some e -> generate_exp context e
  in

  let generate_local_var context Ast.({ var_name=ID id; storage_class; init }) =
    if Context.already_defined context id
    then handle_error (Printf.sprintf "Variable %s declared twice in same scope" id)
    else
      match storage_class with
      | Ast.Extern ->
         (* TODO check this in context? *)
         if init = None then
           Context.add_local_extern_var context id
         else
           handle_error "extern local variable declaration with initializer"
      | Ast.Static ->
         Context.add_static_local_var handle_error context id init
      | Ast.Nothing ->
         let _ = match init with
           | Some exp -> generate_exp context exp
           | None -> () in
         (* push value of var onto stack *)
         let _ = print_asm "    push %eax" in
         Context.add_local_var context id
  in

  let rec generate_statement context statement =
    let context = Context.reset_scope context in
    match statement with
    | Ast.For _  -> generate_for_loop context statement
    | ForDecl _  -> generate_for_decl_loop context statement
    | While _ -> generate_while_loop context statement
    | DoWhile _ -> generate_do_while_loop context statement
    | Block block -> generate_block context block
    | If _ -> generate_if context statement
    | Break -> generate_break context statement; context.var_decl_map
    | Continue -> generate_continue context statement; context.var_decl_map
    | Exp e -> generate_optional_exp context e; context.var_decl_map
    (* for return statements, variable map/stack index unchanged *)
    | ReturnVal exp ->
       let _ = generate_exp context exp in
       emit_function_epilogue ();
       context.var_decl_map
  (*
    for (i = 0; i < 5; i = i + 1) {
        statements
    }

    mov 0, i
_loop:
    cmp i 5
    jmp if false to _post_loop
    do statements
    do i = i + 1
    jmp _loop
_post_loop:
    ...

   *)

  and generate_for_loop context Ast.(For { init ; cond ; post ; body }) =
    begin
      generate_optional_exp context init;
      loop_helper context cond post body
    end

  and generate_for_decl_loop context Ast.(ForDecl { init ; cond ; post ; body }) =
    if init.storage_class = Nothing then
      (* add variable - for loop is new scope *)
      let context' = generate_local_var context init in
      let decl_map = loop_helper context' cond post body in
      (* pop declared variable off the stack *)
      let _ = print_asm "    pop %eax" in
      decl_map
    else
      handle_error "Declared non-local var in for loop"

  and generate_while_loop context Ast.(While { cond ; body }) =
    (* while loops don't have post expression *)
    loop_helper context cond None body

  and loop_helper context cond post body =
    let loop_label = Util.unique_id "loop" in
    let post_loop_label = Util.unique_id "post_loop" in
    let continue_label = Util.unique_id "loop_continue" in
    let _ =
      begin
        emit_label loop_label;
        generate_exp context cond;
        (* jump after loop if cond is false *)
        print_asm "    cmp $0, %eax";
        Printf.fprintf chan "    je %s\n" post_loop_label;
      end
    in
    (* evaluate loop body, which is a new scope *)
    let decl_map =  generate_statement
                      { context with
                        break_label=Some post_loop_label;
                        continue_label=Some continue_label
                      }
                      body
    in
    begin
      emit_label continue_label;
      (* evaluate post expression *)
      generate_optional_exp context post;
      (* execute loop again *)
      Printf.fprintf chan "    jmp %s\n" loop_label;
      (* label end of loop *)
      emit_label post_loop_label;
      decl_map
    end

  and generate_do_while_loop context Ast.(DoWhile { body; cond }) =
    let loop_label = Util.unique_id "do_while" in
    let break_label = Util.unique_id "post_do_while" in
    let continue_label = Util.unique_id "continue_do_while" in
    let _ = emit_label loop_label in
    let decl_map =
      (* do-while body *)
      generate_statement
        { context with break_label=Some break_label;
                       continue_label=Some continue_label
        }
        body
    in
    begin
      emit_label continue_label;
      (* evaluate condition *)
      generate_exp context cond;
      (* jump back to loop if cond is true *)
      print_asm "    cmp $0, %eax";
      Printf.fprintf chan "    jne %s\n" loop_label;
      emit_label break_label;
      decl_map
    end

  and generate_break { break_label; } Ast.Break =
    match break_label with
    | Some label -> Printf.fprintf chan "    jmp %s\n" label
    | None -> handle_error "Break statement not in loop"

  and generate_continue { continue_label; } Ast.Continue =
    match continue_label with
    | Some label -> Printf.fprintf chan "    jmp %s\n" label
    | None -> handle_error "Continue statement not in loop"

  and generate_block context = function
    | [] ->
       let  bytes_to_deallocate = 4 * Set.cardinal context.current_scope in
       (* pop any variables declared in this block off the stack *)
       let _ = Printf.fprintf chan "    addl $%d, %%esp\n" bytes_to_deallocate in
       context.var_decl_map
    | Ast.Statement s::block_items ->
       let decl_map_1 = generate_statement context s in
       let decl_map_2 = generate_block context block_items in
       Map.union decl_map_1 decl_map_2
    | Ast.Decl d::block_items ->
       let context' = generate_local_var context d in
       generate_block context' block_items

  and generate_if context Ast.(If { cond; if_body; else_body }) =
    (* evaluate condition *)
    let _ = generate_exp context cond in
    let post_if_label = Util.unique_id "post_if" in
    let if_decl_map = begin
        (* stuff that's the same whether or not there's an else block *)
        (* compare cond to false *)
        print_asm "    cmp     $0, %eax";
        (* if cond is false, jump over if body *)
        Printf.fprintf chan "    je      %s\n" post_if_label;
        (* generate if body *)
        generate_statement context if_body
      end in
    let else_decl_map = begin
        match else_body with
        (* handle else, if present *)
        | Some else_statement ->
           let post_else_label = Util.unique_id "post_else" in
           let _ =
             begin
               (* We're at end of if statement, need to jump over the else statement *)
               Printf.fprintf chan "    jmp     %s\n" post_else_label;
               (* now print out label after if statement *)
               emit_label post_if_label;
             end
           in
           let decl_map =
             (* now generate else statement *)
             generate_statement context else_statement;
           in
           begin
             (* now print post-else label *)
             emit_label post_else_label;
             decl_map
           end
        | None ->
           begin
             (* print out label that comes after if statement *)
             emit_label post_if_label;
             context.var_decl_map
           end
      end
    in
    Map.union if_decl_map else_decl_map
  in

  let combine_linkages linkg storage_class =
    let open Context in
    match (linkg, storage_class) with
    | Nothing, _ -> failwith "global object has no linkage"
    | External, Ast.Static -> handle_error "static declaration after non-static"
    | Internal, Nothing -> handle_error "non_static declaration after static"
    | _, _ -> linkg
  in

  let update_ctx_for_f ctx f =
    let open Ast in
    let open Context in
    let has_body = f.body <> None in
    let n_params = List.length f.params in
    let storage_class =
      (* 6.2.2.5 *)
      if f.storage_class = Ast.Nothing
      then Extern
      else f.storage_class
    in
    let updated_decl =
      match opt_decl_lookup ctx f.name with
      | None ->
         let linkg =
           if storage_class = Static
           then Internal
           else External
         in
         FunDecl (n_params, linkg, has_body)
      | Some (FunDecl (current_params, current_linkg, current_body)) ->
         if current_params <> n_params then
           handle_error "Function declared with different signatures"
         else if current_body && has_body then
           handle_error "multiple function definitions"
         else
           let linkg = combine_linkages current_linkg storage_class in
           FunDecl (n_params, linkg, current_body || has_body)
      | Some HeapDecl _ -> handle_error "variable redefined as function"
    in
    (Context.add_function ctx f updated_decl, updated_decl)
  in

  let generate_fun global_ctx f =
    let (global_ctx', f_decl) = update_ctx_for_f global_ctx f in
    let Context.FunDecl (_, f_linkg, _) = f_decl in
    let Ast.ID fun_name = f.name in
    begin
      match f.body with
      | Some body ->
         let _ = begin
             print_asm "    .text";
             print_globl_if_extern f_linkg fun_name;
             Printf.fprintf chan "_%s:\n" fun_name;
             print_asm "    push %ebp";
             print_asm "    movl %esp, %ebp"
           end
         in
         let context = Context.init_for_fun global_ctx' f.params in
         let heap_decls = generate_block context body in
         begin
           (* set eax to 0 and generate function epilogue and ret, so function returns 0 even if missing return statement *)
           print_asm "    movl $0, %eax";
           emit_function_epilogue ();
           emit_local_heap_decls heap_decls
         end
      | None -> ()
    end;
    global_ctx'
  in

  let combine_inits prev_init init_val =
    let open Context in
    match prev_init, init_val with
    | NoDef, a -> a
    | a, NoDef -> a
    | Tentative, a -> a
    | a, Tentative -> a
    | Final _, Final _ -> handle_error "multiple variable definitions"
  in
(*
  let update_var_linkage maybe_decl Ast.({ storage_class }) =
    let open Context in
    match maybe_decl with
    | None ->
       begin
         match storage_class with
         | Static -> Internal
         | _ -> External
       end
    | Some (HeapDecl (current_linkage, _)) ->
       combine_linkages current_linkage storage_class
  in
*)
  let generate_global_var context v =
    let open Context in
    let init_val = get_var_init handle_error v in
    let current_decl = opt_decl_lookup context v.var_name in
    let updated_decl =
      match current_decl with
      | None ->
         let linkg =
           if v.storage_class = Static
           then Internal
           else External
         in
         HeapDecl (linkg, init_val)
      | Some (HeapDecl (current_linkage, current_init)) ->
         let linkg = combine_linkages current_linkage v.storage_class in
         let init = combine_inits current_init init_val in
         HeapDecl (linkg, init)
      | Some FunDecl _ -> handle_error "function redefined as variable"
    in
    Context.add_global_var context v updated_decl
  in

  let generate_tl global_ctx = function
      (* function declaration can add variables to heap but not to global scope *)
    | Ast.Function f -> generate_fun global_ctx f
    | Ast.GlobalVar gv -> generate_global_var global_ctx gv
  in

  let rec generate_tls global_ctx = function
    | [] -> Context.(emit_global_heap_decls global_ctx.var_decl_map)
    | tl::tls ->
       (* TODO: use List.fold *)
       let global_ctx' = generate_tl global_ctx tl in
       generate_tls global_ctx' tls
  in

  match prog with
  | Ast.Prog tl_list ->
     let global_ctx = Context.empty in
     let _ = generate_tls global_ctx tl_list in
     close_out chan
