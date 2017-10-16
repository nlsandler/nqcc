open Ast

module Gen : sig
    val generate : AST.prog -> unit
end =
struct



    let generate prog =

        (* Open assembly file for writing *) 
        let chan = open_out "assembly.s" in

        (* main is always entry point *)
        let _ = Printf.fprintf chan "    .globl _main\n" in

        let generate_statement = function
        | AST.Return -> Printf.fprintf chan "ret"
        | AST.ReturnVal(AST.Const(AST.Int i)) -> 
            Printf.fprintf chan "    movl    $%d, %%eax\n" i;
            Printf.fprintf chan "    ret"
        | _ -> failwith("Expression not supported") in

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
