open Batteries

let generate filename prog =

  (* Open assembly file for writing *)
  let filename_asm = String.splice filename (-1) 1 "s" in
  let chan = open_out filename_asm in
  let print_asm = Printf.fprintf chan in

  let handle_error message = begin
      Printf.printf "ERROR: %s\n" message;
      close_out chan;
      Sys.remove filename_asm;
      exit 1;
    end in

  let var_lookup = Context.var_lookup handle_error in

  (* main is always entry point *)
  let _ = Printf.fprintf chan "    .globl _main\n" in

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

  (* generate code to execute expression and move result into eax *)
  let rec generate_exp context = function
    | Ast.(Assign (Equals, (ID id), exp)) ->
       let _ = generate_exp context exp in
       (* get location of variable on stack *)
       let var_index = var_lookup context id in
       (* move value  of eax to that variable *)
       Printf.fprintf chan "    movl %%eax, %d(%%ebp)\n" var_index
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
       let var_index = var_lookup context id in
       (* move value  of variable to eax *)
       Printf.fprintf chan "    movl %d(%%ebp), %%eax\n" var_index;
    | FunCall (ID id, args) ->
       let arg_count = List.length args in
       let _ = put_args_on_stack context args in
       begin
         Printf.fprintf chan "    call _%s\n" id;
         Printf.fprintf chan "    addl $%d, %%esp\n" (arg_count * 4)
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

  let rec generate_statement context statement =
    match statement with
    | Ast.For _  -> generate_for_loop (Context.reset_scope context) statement
    | ForDecl _  -> generate_for_decl_loop (Context.reset_scope context) statement
    | While _ -> generate_while_loop  (Context.reset_scope context) statement
    | DoWhile _ -> generate_do_while_loop (Context.reset_scope context) statement
    | Block block -> generate_block (Context.reset_scope context) block
    | If _ -> generate_if context statement
    | Break -> generate_break context statement
    | Continue -> generate_continue context statement
    | Exp e -> generate_optional_exp context e
    (* for return statements, variable map/stack index unchanged *)
    | ReturnVal exp ->
       let _ = generate_exp context exp in
       emit_function_epilogue ()
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
    (* add variable - for loop is new scope *)
    let context' = generate_declaration context init in
    loop_helper context' cond post body

  and generate_while_loop context Ast.(While { cond ; body }) =
    (* while loops don't have post expression *)
    loop_helper context cond None body

  and loop_helper context cond post body =
    let loop_label = Util.unique_id "loop" in
    let post_loop_label = Util.unique_id "post_loop" in
    let continue_label = Util.unique_id "loop_continue" in
    begin
      emit_label loop_label;
      generate_exp context cond;
      (* jump after loop if cond is false *)
      print_asm "    cmp $0, %%eax\n";
      Printf.fprintf chan "    je %s\n" post_loop_label;
      (* evaluate loop body, which is a new scope *)
      generate_statement { context with break_label=Some post_loop_label;
                                        continue_label=Some continue_label
        }
        body;
      emit_label continue_label;
      (* evaluate post expression *)
      generate_optional_exp context post;
      (* execute loop again *)
      Printf.fprintf chan "    jmp %s\n" loop_label;
      (* label end of loop *)
      emit_label post_loop_label
    end

  and generate_do_while_loop context Ast.(DoWhile { body; cond }) =
    let loop_label = Util.unique_id "do_while" in
    let break_label = Util.unique_id "post_do_while" in
    let continue_label = Util.unique_id "continue_do_while" in
    begin
      (* do-while body *)
      emit_label loop_label;
      generate_statement { context with break_label=Some break_label;
                                        continue_label=Some continue_label
        }
        body;
      emit_label continue_label;
      (* evaluate condition *)
      generate_exp context cond;
      (* jump back to loop if cond is true *)
      print_asm "    cmp $0, %%eax\n";
      Printf.fprintf chan "    jne %s\n" loop_label;
      emit_label break_label
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
    | [] -> ()
    | Ast.Statement s::block_items -> begin
        generate_statement context s;
        generate_block context block_items
      end
    | Ast.Decl d::block_items ->
       let context' = generate_declaration context d in
       generate_block context' block_items

  and generate_if context Ast.(If { cond; if_body; else_body }) =
    (* evaluate condition *)
    let _ = generate_exp context cond in
    let post_if_label = Util.unique_id "post_if" in
    let _ = begin
        (* stuff that's the same whether or not there's an else block *)
        (* compare cond to false *)
        print_asm "    cmp     $0, %%eax\n";
        (* if cond is false, jump over if body *)
        Printf.fprintf chan "    je      %s\n" post_if_label;
        (* generate if body *)
        generate_statement context if_body
      end in
    begin
      match else_body with
      (* handle else, if present *)
      | Some else_statement ->
         let post_else_label = Util.unique_id "post_else" in
         begin
           (* We're at end of if statement, need to jump over the else statement *)
           Printf.fprintf chan "    jmp     %s\n" post_else_label;
           (* now print out label after if statement *)
           emit_label post_if_label;
           (* now generate else statement *)
           generate_statement context else_statement;
           (* now print post-else label *)
           emit_label post_else_label
         end
      | None ->
         (* print out label that comes after if statement *)
         emit_label post_if_label
    end
  in

  let generate_fun Ast.(FunDecl { fun_type; name=ID fun_name; params; body }) =
    let _ = begin
        Printf.fprintf chan "_%s:\n" fun_name;
        print_asm "    push %%ebp\n";
        print_asm "    movl %%esp, %%ebp\n"
      end
    in
    let context = Context.initialize params
    in
    begin
      generate_block context body; (* stack index, i.e. offset of thing after ESP from EBP, is 4 *)
      (* set eax to 0 and generate function epilogue and ret, so function returns 0 even if missing return statement *)
      print_asm "    movl $0, %%eax\n";
      emit_function_epilogue ()
    end
  in

  let rec generate_funs = function
    | [] -> ()
    | f::fs -> generate_fun f; generate_funs fs  in

  match prog with
  | Ast.Prog fun_list ->
     let _ = generate_funs fun_list in
     close_out chan
