(** Given filename 'file.c' and AST, write assembly to 'file.s' **)
val generate : string -> Ast.prog -> unit