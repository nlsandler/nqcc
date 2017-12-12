open Batteries

let generate filename prog =

    (* Open assembly file for writing *) 
    let filename_asm = String.splice filename (-1) 1 "s" in
    let chan = open_out filename_asm in

    (* main is always entry point *)
    let _ = Printf.fprintf chan "    .globl _main\n" in

    (* generate code to execute expression and move result into eax *)
    let rec generate_exp exp var_map stack_index =
        match exp with
        | Ast.BinOp(op, e1, e2) ->
            let _ = generate_exp e1 var_map stack_index in
            let _ = Printf.fprintf chan "    push %%eax\n"  in
            let _ = generate_exp e2 var_map stack_index in
            let _ =
                begin(* op s, d computes d = op(d,s), so put e2 in ecx, e1 in eax *) 
                    Printf.fprintf chan "    movl %%eax, %%ecx\n";
                    (* Put e1 in eax (where it needs to be for idiv) *)
                    Printf.fprintf chan "    pop %%eax\n";
                end in
            (match op with 
            | Ast.Div ->                        
                    (* zero out edx (b/c idivl operand calculates 64-bit value edx:eax / operand) *)
                    Printf.fprintf chan "    xor %%edx, %%edx\n";
                    Printf.fprintf chan "    idivl %%ecx\n";
            | Ast.Sub -> Printf.fprintf chan "    subl %%ecx, %%eax\n"
            | Ast.Add -> Printf.fprintf chan "    addl %%ecx, %%eax\n";
            | Ast.Mult -> Printf.fprintf chan "    imul %%ecx, %%eax\n";
            | _ -> failwith("binop not yet implemented"))
        | Ast.UnOp(op, e) ->
            generate_exp e var_map stack_index;
            (match op with
            | Ast.Pos -> ()(* No-op for now - eventually handle casting to int if needed *)
            | Ast.Negate -> Printf.fprintf chan "    neg %%eax\n";
            | Ast.Complement -> Printf.fprintf chan "    not %%eax\n";
            | Ast.Not -> 
                Printf.fprintf chan "    cmpl $0, %%eax\n";     (* compare eax to 0 *)
                Printf.fprintf chan "    movl $0, %%eax\n";     (* set eax to 0 *)
                Printf.fprintf chan "    sete %%al\n");         (* if eax was zero in earlier comparison, set al to 1 *)
        | Ast.Var(Ast.ID id) ->
            let var_index = Map.find id var_map in
            (* move value  of variable to eax *)
            Printf.fprintf chan "    movl %d(%%ebp), %%eax\n" var_index;
        | Ast.FunCall(Ast.ID id, args) ->
            let arg_count = List.length args in
            let _ = put_args_on_stack args var_map stack_index in
            (* Printf.fprintf chan "    addl    $%d, %%esp\n" (stack_index + 4); *)
            Printf.fprintf chan "    call _%s\n" id;
            Printf.fprintf chan "    addl $%d, %%esp\n" (arg_count * 4);
            (* Printf.fprintf chan "    subl    $%d, %%esp\n" (stack_index + 4); *)
        | Ast.Const(Ast.Int i) -> 
            Printf.fprintf chan "    movl    $%d, %%eax\n" i;
        | Ast.Const(Ast.Char c) ->
            Printf.fprintf chan "    movl    $%d, %%eax\n" (Char.code c);
        | _ -> failwith("Constant not supported") 

    and put_args_on_stack args var_map stack_index =
        let push_arg arg = 
            generate_exp arg var_map stack_index;
            Printf.fprintf chan "    pushl %%eax\n";
        in
        List.iter push_arg (List.rev args)
    (*
        match args with 
        | [] -> ()
        | item::items -> 
             decrement 4 to save a spot on the stack for return value
            let stack_index = stack_index - 4 in 
            generate_exp item var_map stack_index;
            Printf.fprintf chan "    movl    %%eax, %d(%%esp)\n" stack_index;
            put_args_on_stack items var_map (stack_index - 4); *) in

    let rec generate_statement statement var_map stack_index =
        match statement with
        (* for return statements, variable map/stack index unchanged *)
        | Ast.DeclareVar(t, Ast.ID(varname), rhs) ->
            let _ = match rhs with
                | Some exp -> generate_exp exp var_map stack_index
                | None -> () in
            (* push value of var onto stack *)
            let _ = Printf.fprintf chan "    push %%eax\n" in
            let var_map = Map.add varname stack_index var_map in
            let stack_index = stack_index - 4 in
            var_map, stack_index
        | Ast.Assign(Ast.ID(id), exp) ->
            let _ = generate_exp exp var_map stack_index in
            (* get location of variable on stack *)
            let var_index = Map.find id var_map in
            (* move value  of eax to that variable *)
            Printf.fprintf chan "    movl %%eax, %d(%%ebp)\n" var_index;
            (* var_map, stack_index stay the same *)
            var_map, stack_index
        | Ast.If(cond, body, else_body) ->
            (* evaluate condition *)
            let _ = generate_exp cond var_map stack_index in
            let post_if_label = Util.unique_id "post_if" in
            let _ = (* stuff that's the same whether or not there's an else block *)
                (* compare cond to false *)
                Printf.fprintf chan "    cmp     $0, %%eax\n";
                (* if cond is false, jump over if body *)
                Printf.fprintf chan "    je      %s\n" post_if_label;
                (* generate if body *)
                generate_statement_list body var_map stack_index in
            (match else_body with
            | Some else_statements -> 
                let post_else_label = Util.unique_id "post_else" in
                (* We're at end of if statement, need to jump over the else statement *)
                Printf.fprintf chan "    jmp     %s\n" post_else_label;
                (* now print out label after if statement *)
                Printf.fprintf chan "%s:\n" post_if_label;
                (* now generate else statement *)
                generate_statement_list else_statements var_map stack_index;
                (* now print post-else label *)
                Printf.fprintf chan "%s:" post_else_label;
                var_map, stack_index
            | None ->
                (* print out label that comes after if statement *)
                Printf.fprintf chan "%s:\n" post_if_label; 
                var_map, stack_index)
        | Ast.Exp(e) -> generate_exp e var_map stack_index; var_map, stack_index
        | Ast.ReturnVal exp -> 
            let _ = generate_exp exp var_map stack_index in
            begin
                Printf.fprintf chan "    movl %%ebp, %%esp\n";
                Printf.fprintf chan "    pop %%ebp\n";
                Printf.fprintf chan "    ret\n"; 
                var_map, stack_index
            end

    and generate_statement_list statements var_map stack_index =
        match statements with
        | stmt::stmts ->
            let var_map, stack_index = generate_statement stmt var_map stack_index in
            generate_statement_list stmts var_map stack_index
        | [] -> () in

    let generate_fun f = 
        match f with
        | Ast.FunDecl(fun_type, Ast.ID(fun_name), fun_params, Ast.Body(statements)) ->
            let _ = begin
                Printf.fprintf chan "_%s:\n" fun_name;
                Printf.fprintf chan "    push %%ebp\n";
                Printf.fprintf chan "    movl %%esp, %%ebp\n"
            end
            in
            (* arguments are just below return address, which is just below EBP, so first arg at ebp + 8 
                reverse fun_params b/c they go on stack right to left
            *)
            let var_map, stack_index = List.fold_left (fun (m, si) (Ast.Param(_, Ast.ID(id))) -> (Map.add id si m, si + 4)) (Map.empty, 8) (List.rev fun_params) in
            generate_statement_list statements var_map (-4) in (* stack index, i.e. offset of thing after ESP from EBP, is 4 *)

    let rec generate_funs = function
        | [] -> ()
        | f::fs -> generate_fun f; generate_funs fs  in

    match prog with
    | Ast.Prog fun_list ->  
        let _ = generate_funs fun_list in
        close_out chan
