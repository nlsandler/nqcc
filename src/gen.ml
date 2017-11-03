open Batteries

let generate filename prog =

    (* Open assembly file for writing *) 
    let filename_asm = String.splice filename (-1) 1 "s" in
    let chan = open_out filename_asm in

    (* main is always entry point *)
    let _ = Printf.fprintf chan "    .globl _main\n" in

    (* generate code to execute expression and move result into eax *)
    let rec generate_exp exp stack_index =
        match exp with
        | Ast.BinOp(op, e1, e2) ->
            let opcode = 
                match op with
                | Ast.Add -> "addl"
                | Ast.Mult -> "imul"
                | Ast.Div -> "idiv" in
            generate_exp e1 stack_index;
            Printf.fprintf chan "    movl %%eax, %d(%%esp)\n" stack_index;
            generate_exp e2 (stack_index - 4);
            if (op == Ast.Div)
            then
                begin
                    (* idivl calculates 64-bit value edx:eax / operand, so move operands to the right registers *)
                    
                    (* Put e2 into edx temporarily *)
                    Printf.fprintf chan "    movl %%eax, %%edx\n";
                    (* Put e1 in eax (where it needs to be for idiv) *)
                    Printf.fprintf chan "    movl %d(%%esp), %%eax\n" stack_index;
                    (* Now put e2 in esp, overwriting where e1 WAS on the stack *)
                    Printf.fprintf chan "    movl %%edx, %d(%%esp)\n" stack_index;
                    (* zero out edx *)
                    Printf.fprintf chan "    xor %%edx, %%edx\n";
                    (* Actually perform division *)
                    Printf.fprintf chan "    idivl %d(%%esp)\n" stack_index;
                end
            else 
                Printf.fprintf chan "    %s %d(%%esp), %%eax\n" opcode stack_index
        | Ast.Const(Ast.Int i) -> 
            Printf.fprintf chan "    movl    $%d, %%eax\n" i;
        | Ast.Const(Ast.Char c) ->
            Printf.fprintf chan "    movl    $%d, %%eax\n" (Char.code c);
        | _ -> failwith("Constant not supported") in

    let generate_statement = function
    | Ast.Return -> Printf.fprintf chan "    ret"
    | Ast.ReturnVal exp -> 
        let _ = generate_exp exp (-4) in
        Printf.fprintf chan "    ret" in

    let generate_statements statements = List.iter generate_statement statements in

    let generate_fun f = 
        match f with
        | Ast.FunDecl(fun_type, Ast.ID(fun_name), fun_params, Ast.Body(statements)) ->
            let _ = Printf.fprintf chan "_%s:\n" fun_name in
            generate_statements statements in

    match prog with
    | Ast.Prog f ->  
        let _ = generate_fun f in
        close_out chan
