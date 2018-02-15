open Batteries

let rec parse_fun_params = function
    | Tok.CloseParen::rest -> ([], rest)
    | Tok.IntKeyword::(Tok.Id name)::rest -> 
        let other_params, rest = parse_fun_params rest in
        (Ast.Param(Ast.IntType, Ast.ID(name))::other_params, rest)
    | Tok.CharKeyword::(Tok.Id name)::rest -> 
        let other_params, rest = parse_fun_params rest in
        (Ast.Param(Ast.CharType, Ast.ID(name))::other_params, rest)
    | _ -> failwith("Parse error in parse_fun_params")

let tok_to_const = function
    | Tok.Int i -> Ast.Const(Ast.Int i)
    | Tok.Char c -> Ast.Const(Ast.Char c)
    | _ -> failwith("Not a constant")

(* A map from a token to its corresponding Ast.binop.
 * Note that some tokens also correspond to other, non-binop
 * AST elements (e.g. Tok.Minus can also be parsed as unary minus),
 * so only use this map when we're expecting a binop *)
let op_map = Map.empty
    |> Map.add Tok.Plus Ast.Add
    |> Map.add Tok.Minus Ast.Sub
    |> Map.add Tok.Mult Ast.Mult
    |> Map.add Tok.Div Ast.Div
    |> Map.add Tok.Mod Ast.Mod
    |> Map.add Tok.Lt Ast.Lt
    |> Map.add Tok.Le Ast.Le
    |> Map.add Tok.Gt Ast.Gt
    |> Map.add Tok.Ge Ast.Ge
    |> Map.add Tok.DoubleEq Ast.Eq
    |> Map.add Tok.Neq Ast.Neq
    |> Map.add Tok.And Ast.And
    |> Map.add Tok.Or Ast.Or
    |> Map.add Tok.ShiftLeft Ast.ShiftL
    |> Map.add Tok.ShiftRight Ast.ShiftR
    |> Map.add Tok.BitAnd Ast.BitAnd
    |> Map.add Tok.BitOr Ast.BitOr
    |> Map.add Tok.Xor Ast.Xor

(**
 * Parse one production rule for a binary operation.
 * One production rule in the grammar corresponds to a particular precedence level.
 * Parameters:
 * - parse_next_level is a function to parse expressions 
 *   at the next highest precedence level.
 * - op_toks is the list of tokens that should be parsed at this precedence level.
 * - toks is the tokenized source code to parse
 * For example, to parse the production rule for addition and subtraction:
 * <additive-exp> ::= <term> { ("+" | "-") <term> }
 * We would invoke this function as follows:
 * parse_bin_exp parse_term [Tok.Plus; Tok.Minus] toks
 **)
let parse_bin_exp parse_next_level op_toks toks =
    (* call parse_next_level to parse first sub-expression *)
    let left, rest = parse_next_level toks in
    let rec add_terms left_exp toks =
        let hd_tok = List.hd toks in
        if (List.mem hd_tok op_toks) then
            (* The first token after left_exp is an operator we care about,
             * so there must be another sub-expression *)

            (* Parse the next sub-expression *)
            let right_exp, rest = parse_next_level (List.tl toks) in
            let bin_op = Map.find hd_tok op_map in
            (* Construct BinOp AST node BEFORE parsing additional sub-expressions.
             * This enforces left-associativity. *)
            let left_exp = Ast.BinOp(bin_op, left_exp, right_exp) in
            (* Try to parse more sub-expressions *)
            add_terms left_exp rest
        else
            (* No more sub-expressions to add, return current exp *)
            left_exp, toks
        in
    (* try adding more subexpressions *)
    add_terms left rest

let rec parse_function_call = function
    | Tok.Id(name)::Tok.OpenParen::arg_tokens ->
        let fun_name = Ast.ID(name) in
        let args, rest = parse_function_arguments arg_tokens in
        Ast.FunCall(fun_name, args), rest
    | _ -> failwith("Shouldn't have called parse_function_call, this isn't a function call")

and parse_function_arguments = function
    | Tok.CloseParen::rest -> [], rest
    | toks ->
        let arg, rest = parse_exp toks in
        let args, rest = 
            match rest with
            | Tok.Comma::more_args -> parse_function_arguments more_args
            | Tok.CloseParen::after_fun_call -> [], after_fun_call
            | _ -> failwith("Invalid list of function arguments")  in
        arg::args, rest

and parse_factor toks =
    match toks with
    | Tok.OpenParen::factor -> 
        let exp, after_exp = parse_exp factor in
        (match after_exp with
        | (Tok.CloseParen)::rest -> (exp, rest)
        | _ -> failwith("Syntax error: expected close paren"))
    | Tok.Minus::factor ->
        let num, rest = parse_factor factor in
        Ast.UnOp(Ast.Negate, num), rest
    | Tok.Plus::factor ->
        let num, rest = parse_factor factor in
        Ast.UnOp(Ast.Pos, num), rest
    | Tok.Complement::factor ->
        let num, rest = parse_factor factor in
        Ast.UnOp(Ast.Complement, num), rest
    | Tok.Bang::factor ->
        let num, rest = parse_factor factor in
        Ast.UnOp(Ast.Not, num), rest
    | Tok.Id(name)::Tok.OpenParen::rest -> parse_function_call toks
    | Tok.Id(name)::rest -> Ast.Var(Ast.ID(name)), rest
    | Tok.Int(i)::rest -> Ast.Const(Ast.Int(i)), rest
    | Tok.Char(c)::rest -> Ast.Const(Ast.Char(c)), rest
    | _ -> failwith("Failed to parse factor")

and parse_term toks = parse_bin_exp parse_factor [Tok.Mult; Tok.Div; Tok.Mod] toks

and parse_additive_exp toks = parse_bin_exp parse_term [Tok.Plus; Tok.Minus] toks

and parse_shift_exp toks = parse_bin_exp parse_additive_exp [Tok.ShiftLeft; Tok.ShiftRight] toks

and parse_relational_exp toks = parse_bin_exp parse_shift_exp [Tok.Lt; Tok.Le; Tok.Gt; Tok.Ge] toks

and parse_equality_exp toks = parse_bin_exp parse_relational_exp [Tok.DoubleEq; Tok.Neq] toks

and parse_bitwise_and_exp toks = parse_bin_exp parse_equality_exp [Tok.BitAnd] toks

and parse_xor_exp toks = parse_bin_exp parse_bitwise_and_exp [Tok.Xor] toks

and parse_bitwise_or_exp toks = parse_bin_exp parse_xor_exp [Tok.BitOr] toks

and parse_and_exp toks = parse_bin_exp parse_bitwise_or_exp [Tok.And] toks

and parse_or_exp toks = parse_bin_exp parse_and_exp [Tok.Or] toks

and parse_ternary_exp toks =
    let exp_1, rest = parse_or_exp toks in
    match rest with
    | Tok.Question::branch1_tokens ->
        let branch1, rest = parse_exp branch1_tokens in
        begin
        match rest with
        | Tok.Colon::branch2_tokens ->
            let branch2, rest = parse_exp branch2_tokens in
            Ast.TernOp (exp_1, branch1, branch2), rest
        | _ -> failwith("Expected colon after ternary operator")
        end
    | _ -> exp_1, rest

and parse_exp tokens =
    match tokens with
    | Tok.Id(v)::Tok.Eq::rest ->
        (* assignment statement *)
        let var_id = Ast.ID(v) in
        let exp, rest = parse_exp rest in
        Ast.Assign(Ast.Equals, var_id, exp), rest
    | _ -> parse_ternary_exp tokens

let parse_declaration var_type tokens =
    match tokens with
    | Tok.Id(varname)::rest ->
        let var_id = Ast.ID(varname) in
        let init, rest =
            match rest with
            | Tok.Semicolon::rest_of_statements -> None, rest
            | Tok.Eq::rest -> 
                let exp, rest = parse_exp rest in
                Some(exp), rest
            | _ -> failwith("Invalid initial value for variable")
        in 
        let declaration = Ast.{
            var_type = var_type;
            var_name = var_id;
            init = init;
        }
        in
        declaration, rest
    | _ -> failwith("Invalid variable declaration")

let parse_return_statement = function
    | Tok.ReturnKeyword::rest ->
        let exp, rest = parse_exp rest in
        Ast.ReturnVal(exp), rest
    | _ -> failwith("Expected return statement")

let rec parse_if_statement = function
    | Tok.IfKeyword::(Tok.OpenParen::_ as toks) ->
        let cond, rest = parse_exp toks in
        let if_body, rest = parse_statement_or_block rest in
        let else_body, rest =
            match rest with
            | Tok.ElseKeyword::else_tokens ->
                let else_body, rest = parse_statement_or_block else_tokens in
                Some(else_body), rest
            | _ -> None, rest in
        let if_statement = Ast.If(cond, if_body, else_body) in
        if_statement, rest
    | Tok.IfKeyword::_ -> failwith("Expected '(' after 'if'")
    | _ -> failwith("PANIC: parse_if_statement called on non-if statement")

and parse_for_components toks =
    let cond, rest =
        match toks with
        | Tok.Semicolon::next_toks -> parse_exp next_toks
        | _ -> failwith "Expected semicolon in for loop"
    in
    let post, rest =
        match rest with
        | Tok.Semicolon::next_toks -> parse_exp next_toks
        | _ -> failwith "Expected semicolon in for loop"
    in
    let body, rest =
        match rest with
        | Tok.CloseParen::body_toks -> parse_statement_block body_toks
        | _ -> failwith "Expected closing paren in for loop"
    in
    cond, post, body, rest

and parse_for_statement = function
    | Tok.ForKeyword::Tok.OpenParen::Tok.IntKeyword::toks ->
        (* for loop w/ variable declaration *)
        let init, rest = parse_declaration Ast.IntType toks in
        let cond, post, body, rest = parse_for_components rest in
        Ast.ForDecl { init; cond; post; body }, rest
    | Tok.ForKeyword::Tok.OpenParen::toks ->
        let init, rest = parse_exp toks in
        let cond, post, body, rest = parse_for_components rest in
        Ast.For { init; cond; post; body }, rest
    | _ -> failwith("PANIC: not a valid for loop")

and parse_statement_or_block tokens =
    if (List.hd tokens) == Tok.OpenBrace
    then parse_statement_block tokens
    else
        let stmt, rest = parse_statement tokens in
        [stmt], rest

and parse_statement_block = function
    | Tok.OpenBrace::statements ->
        let statement_list, after_list = parse_statement_list statements in
        (match after_list with
        | Tok.CloseBrace::rest -> statement_list, rest
        | _ -> failwith("Expected '}' after statement list"))
    | _ -> failwith("PANIC: parse_statement_block called on non statement block")

(* TODO: actually pay attention to types *)
and parse_statement tokens =
    let stmt, rest =
        (match tokens with
        | Tok.IntKeyword::rest -> 
            let decl, rest = parse_declaration Ast.IntType rest in
            (Ast.Decl decl), rest
        | Tok.CharKeyword::rest -> 
            let decl, rest = parse_declaration Ast.CharType rest in
            (Ast.Decl decl), rest
        | Tok.IfKeyword::rest -> parse_if_statement tokens
        | Tok.ForKeyword::rest -> parse_for_statement tokens
        | Tok.ReturnKeyword::rest -> parse_return_statement tokens
        | _ -> 
            let exp, rest = parse_exp tokens in
            Ast.Exp(exp), rest) in
    match stmt, rest with
    (* statement blocks have no semicolons after them *)
    | Ast.If _, _ -> stmt, rest
    | Ast.For _, _ -> stmt, rest
    | Ast.ForDecl _, _ -> stmt, rest
    (* expressions & declarations do *)
    | _, Tok.Semicolon::rest -> stmt, rest (* eat semicolon from end of statement *)
    | _ -> failwith("Expected semicolon at end of statement") 

and parse_statement_list tokens =
    if (List.hd tokens) == Tok.CloseBrace
    then [], tokens
    else
        let next_statement, rest = parse_statement tokens in
        let statements, rest = parse_statement_list rest in
        next_statement::statements, rest        

let parse_fun_body tokens = (* Assume for now there's nothing after function body *)
    let statements, rest = parse_statement_list tokens in
    match rest with
    | Tok.CloseBrace::rest -> Ast.Body(statements), rest
    | _ -> failwith("Expected closing brace")

let parse_fun tokens =
    let fun_type, fun_name, rest = 
        match tokens with
        | Tok.IntKeyword::(Tok.Id name)::Tok.OpenParen::rest -> (Ast.IntType, Ast.ID(name), rest)
        | Tok.CharKeyword::(Tok.Id name)::Tok.OpenParen::rest -> (Ast.CharType, Ast.ID(name), rest)
        | _ -> failwith("Parse error in parse_fun: bad function type or name") in
    let fun_params, rest = parse_fun_params rest in
    let fun_body, rest = 
        match rest with
        | Tok.OpenBrace::rest -> parse_fun_body rest
        | _ -> failwith("Expected brace to open function body") in
    Ast.FunDecl(fun_type, fun_name, fun_params, fun_body), rest

let rec parse_funs = function
    | [] -> [] (* no functions left to parse *)
    | tokens ->
        let f, rest = parse_fun tokens in
        let fs = parse_funs rest in
        f::fs

let parse tokens = Ast.Prog(parse_funs tokens)