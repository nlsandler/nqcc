open Batteries

let generate filename prog =

  (* Open assembly file for writing *)
  let filename_asm = String.splice filename (-1) 1 "s" in
  let chan = open_out filename_asm in
  (* TODO: add newline! *)
  let print_asm = Printf.fprintf chan in

  let handle_error message = begin
      Printf.printf "ERROR: %s\n" message;
      close_out chan;
      Sys.remove filename_asm;
      exit 1;
    end in

  let var_lookup = Context.var_lookup handle_error in

  let emit_label label = Printf.fprintf chan "%s:\n" label in

  let emit_comparison set_instruction =
    begin
      print_asm "    cmp %%ecx, %%eax\n";
      print_asm "    movl $0, %%eax\n";
      Printf.fprintf chan "    %s %%al\n" set_instruction
    end in

  let emit_function_epilogue () =
    begin
      print_asm "    movl %%ebp, %%esp\n";
      print_asm "    pop %%ebp\n";
      print_asm "    ret\n"
    end in

  let emit_bin_op op =
    let open Ast in
    match op with
    | Div ->
       (* zero out edx (b/c idivl operand calculates 64-bit value edx:eax / operand) *)
       begin
         print_asm "    xor %%edx, %%edx\n";
         print_asm "    idivl %%ecx\n";
       end
    | Mod ->
       (* zero out edx (b/c idivl operand calculates 64-bit value edx:eax / operand) *)
       begin
         print_asm "    xor %%edx, %%edx\n";
         print_asm "    idivl %%ecx\n";
         (* remainder stored in edx, move it to eax *)
         print_asm "    movl %%edx, %%eax\n"
       end
    | Sub -> print_asm "    subl %%ecx, %%eax\n"
    | Add -> print_asm "    addl %%ecx, %%eax\n";
    | Mult -> print_asm "    imul %%ecx, %%eax\n";
    | Xor -> print_asm "    xor %%ecx, %%eax\n";
    | BitAnd -> print_asm "    and %%ecx, %%eax\n";
    | BitOr -> print_asm "    or %%ecx, %%eax\n";
    | ShiftL -> print_asm "    sall %%cl, %%eax\n";
    | ShiftR -> print_asm "    sarl %%cl, %%eax\n";
    | Eq -> emit_comparison "sete"
    | Neq -> emit_comparison "setne"
    | Lt -> emit_comparison "setl"
    | Le -> emit_comparison "setle"
    | Gt -> emit_comparison "setg"
    | Ge -> emit_comparison "setge"
    | Or ->
       begin
         print_asm "    orl %%eax, %%ecx\n";
         print_asm "    movl $0, %%eax\n";
         print_asm "    setne %%al\n"
       end
    | And ->
       begin
         (* if eax != 0, set al = 1 *)
         print_asm "    cmp $0, %%eax\n";
         print_asm "    movl $0, %%eax\n"; (* zero this b/c we'll store result in it*)
         print_asm "    setne %%al\n";
         (* if ecx != 0, set cl = 1 *)
         print_asm "    cmp $0, %%ecx\n";
         print_asm "    setne %%cl\n";
         (* eax = al && cl *)
         print_asm "    andb %%cl, %%al\n"
       end in

  (* TODO refactor a lot of these print functions, e.g. align *)
  let emit_local_heap_decl lbl heap_decl =
    let Context.(HeapDecl (_, Final init)) = heap_decl in
    if init = 0 then
      begin
        print_asm "    .text\n";
        Printf.fprintf chan "    .zerofill __DATA,__bss,_%s,4,2\n" lbl
      end
    else
      begin
        print_asm "    .data\n";
        print_asm "    .align 2\n";
        Printf.fprintf chan "_%s:\n" lbl;
        Printf.fprintf chan "    .long %d\n" init
      end
  in

  let emit_local_heap_decls = Map.iter emit_local_heap_decl in

  let emit_global_heap_decl lbl (Context.HeapDecl (linkg, init)) =
    let open Context in
    let print_globl_if_extern _ =
      if linkg = External
      then Printf.fprintf chan "    .globl _%s\n" lbl
      else ()
    in
    let init' = finalize_init init in
    match init' with
    | NoDef -> ()
    | Final 0 ->
       if linkg = External then
         (* allocate space in .comm - NOTE different on Linux! *)
         begin
           print_asm "    .text\n";
           Printf.fprintf chan "    .comm _%s,4,2\n" lbl
         end
       else
         (* allocate space in bss *)
         begin
           print_asm "    .text\n";
           Printf.fprintf chan "    .zerofill __DATA,__bss,_%s,4,2\n" lbl
         end
    | Final i ->
       begin
         print_asm "    .text\n";
         print_globl_if_extern ();
         print_asm "    .data\n";
         print_asm "    .align 2\n";
         Printf.fprintf chan "_%s:\n" lbl;
         Printf.fprintf chan "    .long %d\n" i
       end
  in

  let emit_global_heap_decls = Map.iter emit_global_heap_decl in

  (* generate code to execute expression and move result into eax *)
  let rec generate_exp context = function
    | Ast.(Assign (Equals, (ID id), exp)) ->
       let _ = generate_exp context exp in
       (* get location of variable in memory *)
       begin
         match var_lookup context id with
         (* TODO refactor these liens *)
         | Context.Stack var_index -> Printf.fprintf chan "    movl %%eax, %d(%%ebp)\n" var_index
         | Context.Heap lbl -> Printf.fprintf chan "    movl %%eax, _%s\n" lbl
       end

    | TernOp (e1, e2, e3) ->
       let post_tern_label = Util.unique_id "post_ternary" in
       let e3_label = Util.unique_id "second_branch_label" in
       begin
         (* calculate cond *)
         generate_exp context e1;
         (* is cond true? *)
         print_asm "    cmp   $0, %%eax\n";
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
         print_asm "    push %%eax\n";
         generate_exp context e2;
         (* op s, d computes d = op(d,s), so put e2 in ecx, e1 in eax *)
         print_asm "    movl %%eax, %%ecx\n";
         (* Put e1 in eax (where it needs to be for idiv) *)
         print_asm  "    pop %%eax\n";
         (* perform operation *)
         emit_bin_op op
       end
    | UnOp (op, e) ->
       generate_exp context e;
       begin
         match op with
         | Pos -> ()(* No-op for now - eventually handle casting to int if needed *)
         | Negate -> print_asm "    neg %%eax\n";
         | Complement -> print_asm "    not %%eax\n";
         | Not ->
            print_asm "    cmpl $0, %%eax\n";     (* compare eax to 0 *)
            print_asm "    movl $0, %%eax\n";     (* set eax to 0 *)
            print_asm "    sete %%al\n";         (* if eax was zero in earlier comparison, set al to 1 *)
       end
    | Var (ID id) ->
       begin
         (* move value  of variable to eax *)
         match var_lookup context id with
         (* TODO refactor these lines *)
         | Context.Stack var_index -> Printf.fprintf chan "    movl %d(%%ebp), %%eax\n" var_index
         | Context.Heap lbl -> Printf.fprintf chan "    movl _%s, %%eax\n" lbl
       end
    | FunCall (ID id, args) ->
       let arg_count = List.length args in
       let _ =
         (* edx = (esp - 4*(arg_count + 1)) % 16 *)
         (* the + 1 is for saved remainder *)
         print_asm "    movl %%esp, %%eax\n";
         Printf.fprintf chan "    subl $%d, %%eax\n" (4*(arg_count + 1));
         print_asm "    xorl %%edx, %%edx\n";
         print_asm "    movl $0x20, %%ecx\n";
         print_asm "    idivl %%ecx\n";
         (* edx contains the remainder, i.e. # of bytes to subtract *)
         print_asm "    subl %%edx, %%esp\n";
         print_asm "    pushl %%edx\n" (* need it for deallocating *)
       in
       let _ = put_args_on_stack context args in
       begin
         (* actually make the call *)
         Printf.fprintf chan "    call _%s\n" id;
         (* deallocate args *)
         Printf.fprintf chan "    addl $%d, %%esp\n" (arg_count * 4);
         (* pop remainder off stack, undo 16-byte alignment *)
         print_asm "    popl %%edx\n";
         print_asm "    addl %%edx, %%esp\n"
       end
    | Const (Int i) ->
      Printf.fprintf chan "    movl    $%d, %%eax\n" i
    | Const (Char c) ->
       Printf.fprintf chan "    movl    $%d, %%eax\n" (Char.code c)
    | _ -> handle_error "Constant not supported"

  and put_args_on_stack context args =
    let push_arg arg =
      generate_exp context arg;
      print_asm "    pushl %%eax\n";
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
           Context.add_extern_var context id
         else
           handle_error "extern local variable declaration with initializer"
      | Ast.Static ->
         Context.add_static_local_var handle_error context id init
      | Ast.Nothing ->
         let _ = match init with
           (* TODO refactor generating initializer *)
           | Some exp -> generate_exp context exp
           | None -> () in
         (* push value of var onto stack *)
         let _ = print_asm "    push %%eax\n" in
         Context.add_local_var context id
  in

(*
  let generate_declaration context Ast.({ var_type; init; var_name=ID id; }) =
    if Context.already_defined context id
    then handle_error (Printf.sprintf "Variable %s declared twice in same scope" id)
    else
      let _ = match init with
        | Some exp -> generate_exp context exp
        | None -> () in
      (* push value of var onto stack *)
      let _ = print_asm "    push %%eax\n" in
      Context.add_var context id
  in
*)
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
      let _ = print_asm "    pop %%eax\n" in
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
        print_asm "    cmp $0, %%eax\n";
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
      print_asm "    cmp $0, %%eax\n";
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
        print_asm "    cmp     $0, %%eax\n";
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

  let generate_fun global_ctx Ast.(FunDecl { fun_type; name=ID fun_name; params; body }) =
    match body with
    | Some body ->
       let _ = begin
           print_asm "    .text\n";
           Printf.fprintf chan "    .globl _%s\n" fun_name;
           Printf.fprintf chan "_%s:\n" fun_name;
           print_asm "    push %%ebp\n";
           print_asm "    movl %%esp, %%ebp\n"
         end
       in
       let context = Context.init_for_fun global_ctx params in
       let heap_decls = generate_block context body in
       begin
         (* set eax to 0 and generate function epilogue and ret, so function returns 0 even if missing return statement *)
         print_asm "    movl $0, %%eax\n";
         emit_function_epilogue ();
         emit_local_heap_decls heap_decls
       end
    | None -> ()
  in

  let combine_inits maybe_decl v =
    let open Context in
    let init_val = get_var_init handle_error v in
    match maybe_decl with
    | None -> init_val
    | Some (HeapDecl (_, prev_init)) ->
       begin
         match prev_init, init_val with
         | NoDef, a -> a
         | a, NoDef -> a
         | Tentative, a -> a
         | a, Tentative -> a
         | Final _, Final _ -> handle_error "multiple variabled definitions"
       end
  in

  let combine_linkage maybe_decl Ast.({ storage_class }) =
    Ast.(
      Context.(
        match maybe_decl with
        | None ->
           begin
             match storage_class with
             | Static -> Internal
             | _ -> External
           end
      | Some (HeapDecl (current_linkage, _)) ->
         begin
           match current_linkage, storage_class with
           | Nothing, _ -> failwith "global var has no linkage"
           | External, Static -> handle_error "static declaration after non-static"
           | Internal, Nothing -> handle_error "non_static declaration after static"
           | _, _ -> current_linkage
         end
      )
    )
  in

  let generate_global_var context v =
    let current_decl = Context.opt_var_decl_lookup context v in
    let init = combine_inits current_decl v in
    let linkage = combine_linkage current_decl v in
    let updated_decl = Context.HeapDecl (linkage, init) in
    Context.add_global_var context v updated_decl
  in

  let generate_tl global_ctx = function
      (* function declaration can add variables to heap but not to global scope *)
    | (Ast.FunDecl _) as f -> generate_fun global_ctx f; global_ctx
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
