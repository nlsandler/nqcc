open Batteries

let generate filename prog =

    (* Open assembly file for writing *) 
    let filename_asm = String.splice filename (-1) 1 "s" in
    let chan = open_out filename_asm in

    (* main is always entry point *)
    let _ = Printf.fprintf chan "    .globl _main\n" in

    (* generate code to execute expression and move result into eax *)
    let generate_exp = function
    | Ast.Const(Ast.Int i) -> 
        Printf.fprintf chan "    movl    $%d, %%eax\n" i;
    | Ast.Const(Ast.Char c) ->
        Printf.fprintf chan "    movl    $%d, %%eax\n" (Char.code c);
    | _ -> failwith("Constant not supported") in

    let generate_statement = function
    | Ast.Return -> Printf.fprintf chan "    ret"
    | Ast.ReturnVal exp -> 
        let _ = generate_exp exp in
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
