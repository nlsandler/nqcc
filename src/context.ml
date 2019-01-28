open Batteries

type linkage =
  | External
  | Internal
  | Nothing

type initial =
  | Final of int
  | Tentative (* defaults to 0 *)
  | NoDef

type address =
  | Stack of int   (* offset on stack *)
  | Heap of string (* label *)

type heap_decl = HeapDecl of linkage * initial

(* A record for all the stuff we have to pass around during code generation *)
type t = {
    (* map variable name to address *)
    var_map        : (string, address) Map.t;
    (* map memory location label to declaration info for vars on heap *)
    var_decl_map   : (string, heap_decl) Map.t;
    current_scope  : string Set.t;
    stack_index    : int;
    break_label    : string option;
    continue_label : string option
}

let empty =
  { var_decl_map=Map.empty;
    var_map=Map.empty;
    (* not used at global scope *)
    current_scope=Set.empty;
    stack_index=0;
    break_label=None;
    continue_label=None;
}

(* Add function arguments to initial context with just global variables *)
let init_for_fun { var_map } params =
  (* arguments are just below return address, which is just below EBP, so first arg at ebp + 8
   *)
  let add_param (m, si, sc) (Ast.Param (_, ID id)) =
    Map.add id (Stack si) m, si + 4, Set.add id sc
  in
  let var_map', _, scope =
    List.fold_left
      add_param
      (var_map, 8, Set.empty)
      params
  in
  { var_decl_map=Map.empty;
    var_map=var_map';
    current_scope=scope;
    (* stack index, i.e. offset of thing after ESP from EBP, is 4 *)
    stack_index=(-4);
    break_label=None;
    continue_label=None
  }

let already_defined { current_scope; } id = Set.mem id current_scope
let reset_scope context = { context with current_scope=Set.empty }

(* TODO: prob doesn't belong here since gen.ml uses it *)
let get_init on_err = function
  | None -> Tentative
  | Some (Ast.Const c) ->
     begin
       match c with
       | Ast.Int i -> Final i
       (* TODO: better error handling here! *)
       | _ -> on_err "non-int initializers not implemented"
     end
  | _ -> on_err "non-constant initializer"

let get_var_init on_err Ast.({ init; storage_class; }) =
  match (get_init on_err init) with
  | Tentative ->
     if storage_class = Ast.Extern
     then NoDef
     else Tentative
  | x -> x

let finalize_init = function
  | Tentative -> Final 0
  | a -> a

let add_extern_var ({ var_map; current_scope; } as context) id =
  let var_label = "_"^id in
  { context with
    var_map=Map.add id (Heap var_label) var_map;
    current_scope=Set.add id current_scope;
  }

let add_global_var ({ var_map; var_decl_map } as context) Ast.({ var_name=ID id }) decl =
  let label = "_"^id in
  { context with
    var_map=Map.add id (Heap label) var_map;
    var_decl_map=Map.add label decl var_decl_map
  }

let add_static_local_var on_err ({ var_map; var_decl_map; current_scope; } as context) id init =
  let label = Util.unique_id id in
  let init' = finalize_init (get_init on_err init) in
  { context with
    var_map=Map.add id (Heap label) var_map;
    var_decl_map=Map.add label (HeapDecl (Internal, init')) var_decl_map;
    current_scope=Set.add id current_scope;
  }

let add_local_var ({ var_map; current_scope; stack_index; } as context) id =
  let stack_index', addr = stack_index - 4, Stack stack_index in
  { context with
    var_map=Map.add id addr var_map;
    current_scope=Set.add id current_scope;
    stack_index=stack_index';
  }

let var_lookup err_handler { var_map } var =
  try
    Map.find var var_map
  with
  | Not_found -> err_handler (Printf.sprintf "undeclared variable %s" var)

let opt_var_decl_lookup { var_decl_map } Ast.({ var_name=ID id }) =
  try
    Some (Map.find ("_"^id) var_decl_map)
  with
    | Not_found -> None
