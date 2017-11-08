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
            let _ = Printf.fprintf chan "    movl %%eax, %d(%%esp)\n" stack_index in
            let _ = generate_exp e2 var_map (stack_index - 4) in
            let _ =
                begin(* op s, d computes d = op(d,s), so swap eax w/ stack *) 
                    Printf.fprintf chan "    movl %%eax, %%edx\n";
                    (* Put e1 in eax (where it needs to be for idiv) *)
                    Printf.fprintf chan "    movl %d(%%esp), %%eax\n" stack_index;
                    (* Now put e2 in esp, overwriting where e1 WAS on the stack *)
                    Printf.fprintf chan "    movl %%edx, %d(%%esp)\n" stack_index;
                end in
            (match op with 
            | Ast.Div ->                        
                    (* zero out edx (b/c idivl operand calculates 64-bit value edx:eax / operand) *)
                    Printf.fprintf chan "    xor %%edx, %%edx\n";
                    Printf.fprintf chan "    idivl %d(%%esp)\n" stack_index;
            | Ast.Sub -> Printf.fprintf chan "    subl %d(%%esp), %%eax\n" stack_index;
            | Ast.Add -> Printf.fprintf chan "    addl %d(%%esp), %%eax\n" stack_index;
            | Ast.Mult -> Printf.fprintf chan "    imul %d(%%esp), %%eax\n" stack_index;)
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
            Printf.fprintf chan "    movl %d(%%esp), %%eax\n" var_index;
        | Ast.Const(Ast.Int i) -> 
            Printf.fprintf chan "    movl    $%d, %%eax\n" i;
        | Ast.Const(Ast.Char c) ->
            Printf.fprintf chan "    movl    $%d, %%eax\n" (Char.code c);
        | _ -> failwith("Constant not supported") in

    let generate_statement statement var_map stack_index =
    match statement with
    (* for return statements, variable map/stack index unchanged *)
    | Ast.DeclareVar(t, Ast.ID(varname), rhs) ->
        let _ = match rhs with
            | Some exp -> generate_exp exp var_map stack_index
            | None -> () in
        (* push value of var onto stack *)
        let _ = Printf.fprintf chan "    movl %%eax, %d(%%esp)\n" stack_index in
        let var_map = Map.add varname stack_index var_map in
        let stack_index = stack_index - 4 in
        var_map, stack_index
    | Ast.Assign(Ast.ID(id), exp) ->
        let _ = generate_exp exp var_map stack_index in
        (* get location of variable on stack *)
        let var_index = Map.find id var_map in
        (* move value  of eax to that variable *)
        Printf.fprintf chan "    movl %%eax, %d(%%esp)\n" var_index;
        (* var_map, stack_index stay the same *)
        var_map, stack_index
    | Ast.Return -> Printf.fprintf chan "    ret\n"; var_map, stack_index
    | Ast.ReturnVal exp -> 
        let _ = generate_exp exp var_map stack_index in
        Printf.fprintf chan "    ret\n"; var_map, stack_index in

    let rec generate_statement_list statements var_map stack_index =
        match statements with
        | stmt::stmts ->
            let var_map, stack_index = generate_statement stmt var_map stack_index in
            generate_statement_list stmts var_map stack_index
        | [] -> () in

    let generate_fun f = 
        match f with
        | Ast.FunDecl(fun_type, Ast.ID(fun_name), fun_params, Ast.Body(statements)) ->
            let _ = Printf.fprintf chan "_%s:\n" fun_name in
            generate_statement_list statements Map.empty (-4) in

    match prog with
    | Ast.Prog f ->  
        let _ = generate_fun f in
        close_out chan
