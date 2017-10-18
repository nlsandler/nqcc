open Ast
open Batteries

module Gen : sig
    val generate : string -> AST.prog -> unit
end =
struct

    let generate filename prog =

        (* Open assembly file for writing *) 
        let filename_asm = String.splice filename (-1) 1 "s" in
        let chan = open_out filename_asm in

        (* main is always entry point *)
        let _ = Printf.fprintf chan "    .globl _main\n" in

        (* generate code to execute expression and move result into eax *)
        let generate_exp = function
        | AST.Const(AST.Int i) -> 
            Printf.fprintf chan "    movl    $%d, %%eax\n" i;
        | AST.Const(AST.Char c) ->
            Printf.fprintf chan "    movl    $%d, %%eax\n" (Char.code c);
        | _ -> failwith("Constant not supported") in

        let generate_statement = function
        | AST.Return -> Printf.fprintf chan "    ret"
        | AST.ReturnVal exp -> 
            let _ = generate_exp exp in
            Printf.fprintf chan "    ret" in

        let generate_statements statements = List.iter generate_statement statements in

        let generate_fun f = 
            match f with
            | AST.FunDecl(fun_type, AST.ID(fun_name), fun_params, AST.Body(statements)) ->
                let _ = Printf.fprintf chan "_%s:\n" fun_name in
                generate_statements statements in
        
        match prog with
        | AST.Prog f ->  
            let _ = generate_fun f in
            close_out chan
end
