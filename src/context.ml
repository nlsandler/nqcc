open Batteries

(* A record for all the stuff we have to pass around during code generation *)
type t = {
    var_map        : (string, int) Map.t;
    current_scope  : string Set.t;
    stack_index    : int;
    break_label    : string option;
    continue_label : string option
}

let initialize params =
  (* arguments are just below return address, which is just below EBP, so first arg at ebp + 8
     reverse fun_params b/c they go on stack right to left
   *)
  let add_param (m, si, sc) (Ast.Param (_, ID id)) =
    Map.add id si m, si + 4, Set.add id sc
  in
  let var_map, _, scope =
    List.fold_left
      add_param
      (Map.empty, 8, Set.empty)
      (List.rev params)
  in
  {
    var_map=var_map;
    current_scope=scope;
    stack_index=(-4);
    break_label=None;
    continue_label=None
  }

let already_defined { current_scope; } id = Set.mem id current_scope
let reset_scope context = { context with current_scope=Set.empty }

let add_var ({ var_map; current_scope; stack_index } as context) id =
  let var_map' = Map.add id stack_index var_map in
  let current_scope' = Set.add id current_scope in
  { context with var_map=var_map';
                 current_scope=current_scope';
                 stack_index=stack_index - 4
  }

let var_lookup err_handler { var_map } var =
  try
    Map.find var var_map
  with
  | Not_found -> err_handler (Printf.sprintf "undeclared variable %s" var)
