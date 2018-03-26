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

  let safe_map_lookup var var_map =
    try
      Map.find var var_map
    with
    | Not_found -> handle_error (Printf.sprintf "undeclared variable %s" var) in

  (* main is always entry point *)
  let _ = Printf.fprintf chan "    .globl _main\n" in

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
  let rec generate_exp exp var_map =
    let open Ast in
    match exp with
    | Assign (Equals, (ID id), exp) ->
       let _ = generate_exp exp var_map in
       (* get location of variable on stack *)
       let var_index = safe_map_lookup id var_map in
       (* move value  of eax to that variable *)
       Printf.fprintf chan "    movl %%eax, %d(%%ebp)\n" var_index
    | TernOp (e1, e2, e3) ->
       let post_tern_label = Util.unique_id "post_ternary" in
       let e3_label = Util.unique_id "second_branch_label" in
       begin
         (* calculate cond *)
         generate_exp e1 var_map;
         (* is cond true? *)
         print_asm "    cmp   $0, %%eax\n";
         (* if it's false, jump to 'else' exp *)
         Printf.fprintf chan "    je   %s\n" e3_label;
         (* Generate 'if' block *)
         generate_exp e2 var_map;
         (* after 'if', jump over 'else' *)
         Printf.fprintf chan "    jmp    %s\n" post_tern_label;
         (* Label start of 'else' *)
         Printf.fprintf chan "%s:\n" e3_label;
         (* Generate 'else' *)
         generate_exp e3 var_map;
         (* Label end of ternary operation *)
         Printf.fprintf chan "%s:\n" post_tern_label
       end
    | BinOp (op, e1, e2) ->
       begin
         (* calculate e1 and e2 *)
         generate_exp e1 var_map;
         print_asm "    push %%eax\n";
         generate_exp e2 var_map;
         (* op s, d computes d = op(d,s), so put e2 in ecx, e1 in eax *)
         print_asm "    movl %%eax, %%ecx\n";
         (* Put e1 in eax (where it needs to be for idiv) *)
         print_asm  "    pop %%eax\n";
         (* perform operation *)
         emit_bin_op op
       end
    | UnOp (op, e) ->
       generate_exp e var_map;
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
       let var_index = safe_map_lookup id var_map in
       (* move value  of variable to eax *)
       Printf.fprintf chan "    movl %d(%%ebp), %%eax\n" var_index;
    | FunCall (ID id, args) ->
       let arg_count = List.length args in
       let _ = put_args_on_stack args var_map in
       begin
         Printf.fprintf chan "    call _%s\n" id;
         Printf.fprintf chan "    addl $%d, %%esp\n" (arg_count * 4)
       end
    | Const (Int i) ->
       Printf.fprintf chan "    movl    $%d, %%eax\n" i
    | Const (Char c) ->
       Printf.fprintf chan "    movl    $%d, %%eax\n" (Char.code c)
    | _ -> failwith "Constant not supported"

  and put_args_on_stack args var_map =
    let push_arg arg =
      generate_exp arg var_map;
      print_asm "    pushl %%eax\n";
    in
    List.iter push_arg (List.rev args)
  in

  let generate_optional_exp var_map = function
    | None -> ()
    | Some e -> generate_exp e var_map
  in

  let generate_declaration Ast.({ var_type; init; var_name=ID id; }) var_map current_scope stack_index =
    if Set.mem id current_scope
    then handle_error (Printf.sprintf "Variable %s declared twice in same scope" id)
    else
      let _ = match init with
        | Some exp -> generate_exp exp var_map
        | None -> () in
      (* push value of var onto stack *)
      let _ = print_asm "    push %%eax\n" in
      let var_map = Map.add id stack_index var_map in
      let new_scope = Set.add id current_scope in
      let stack_index = stack_index - 4 in
      var_map, new_scope, stack_index
  in

  let rec generate_statement statement var_map current_scope stack_index =
    let open Ast in
    match statement with
    | For _  -> generate_for_loop statement var_map stack_index
    | ForDecl _  -> generate_for_decl_loop statement var_map stack_index
    | While _ -> generate_while_loop statement var_map stack_index
    | DoWhile _ -> generate_do_while_loop statement var_map stack_index
    | Block block -> generate_block var_map Set.empty stack_index block
    | If _ -> generate_if statement var_map current_scope stack_index
    | Exp e -> generate_optional_exp var_map e
    (* for return statements, variable map/stack index unchanged *)
    | ReturnVal exp ->
       let _ = generate_exp exp var_map in
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

  and generate_for_loop Ast.(For { init ; cond ; post ; body }) var_map stack_index =
    begin
      generate_optional_exp var_map init;
      loop_helper cond post body var_map stack_index
    end

  and generate_for_decl_loop Ast.(ForDecl { init ; cond ; post ; body }) var_map stack_index =
    (* add variable - for loop is new scope*)
    let var_map', _, stack_index' = generate_declaration init var_map Set.empty stack_index in
    loop_helper cond post body var_map' stack_index'

  and generate_while_loop Ast.(While { cond ; body }) var_map stack_index =
    (* while loops don't have post expression *)
    loop_helper cond None body var_map stack_index

  and loop_helper cond post body var_map stack_index =
    let loop_label = Util.unique_id "loop" in
    let post_loop_label = Util.unique_id "post_loop" in
    begin
      Printf.fprintf chan "%s:\n" loop_label;
      generate_exp cond var_map;
      (* jump after loop if cond is false *)
      print_asm "    cmp $0, %%eax\n";
      Printf.fprintf chan "    je %s\n" post_loop_label;
      (* evaluate loop body, which is a new scope *)
      generate_statement body var_map Set.empty stack_index;
      (* evaluate post expression *)
      generate_optional_exp var_map post;
      (* execute loop again *)
      Printf.fprintf chan "    jmp %s\n" loop_label;
      (* label end of loop *)
      Printf.fprintf chan "%s:\n" post_loop_label
    end

  and generate_do_while_loop Ast.(DoWhile { body; cond }) var_map stack_index =
    let loop_label = Util.unique_id "do_while" in
    begin
      (* do-while body *)
      Printf.fprintf chan "%s:\n" loop_label;
      generate_statement body var_map Set.empty stack_index;
      (* evaluate condition *)
      generate_exp cond var_map;
      (* jump back to loop if cond is true *)
      print_asm "    cmp $0, %%eax\n";
      Printf.fprintf chan "    jne %s\n" loop_label
    end

  and generate_block var_map current_scope stack_index = function
    | [] -> ()
    | Ast.Statement s::block_items -> begin
        generate_statement s var_map current_scope stack_index;
        generate_block var_map current_scope stack_index block_items
      end
    | Ast.Decl d::block_items ->
       let var_map', current_scope', stack_index' = generate_declaration d var_map current_scope stack_index in
       generate_block var_map' current_scope' stack_index' block_items

  and generate_if Ast.(If { cond; if_body; else_body }) var_map current_scope stack_index =
    (* evaluate condition *)
    let _ = generate_exp cond var_map in
    let post_if_label = Util.unique_id "post_if" in
    let _ = begin
        (* stuff that's the same whether or not there's an else block *)
        (* compare cond to false *)
        print_asm "    cmp     $0, %%eax\n";
        (* if cond is false, jump over if body *)
        Printf.fprintf chan "    je      %s\n" post_if_label;
        (* generate if body *)
        generate_statement if_body var_map Set.empty stack_index
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
           Printf.fprintf chan "%s:\n" post_if_label;
           (* now generate else statement *)
           generate_statement else_statement var_map Set.empty stack_index;
           (* now print post-else label *)
           Printf.fprintf chan "%s:" post_else_label
         end
      | None ->
         (* print out label that comes after if statement *)
         Printf.fprintf chan "%s:\n" post_if_label
    end
  in

  let generate_fun Ast.(FunDecl { fun_type; name=ID fun_name; params; body }) =
    let _ = begin
        Printf.fprintf chan "_%s:\n" fun_name;
        print_asm "    push %%ebp\n";
        print_asm "    movl %%esp, %%ebp\n"
      end
    in
    (* arguments are just below return address, which is just below EBP, so first arg at ebp + 8
         reverse fun_params b/c they go on stack right to left
     *)
    let var_map, stack_index = List.fold_left
                                 (fun (m, si) Ast.(Param (_, ID id)) ->
                                   (Map.add id si m, si + 4))
                                 (Map.empty, 8)
                                 (List.rev params)
    in
    begin
      generate_block var_map Set.empty (-4) body; (* stack index, i.e. offset of thing after ESP from EBP, is 4 *)
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
