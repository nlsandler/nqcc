open Batteries

type linkage =
  | External
  | Internal
  | Nothing

type address =
  | Stack of int   (* offset on stack *)
  | Heap of string (* label *)

type heap_decl = HeapDecl of linkage * int option (* linkage, init *)

let init_or_zero (HeapDecl (_, init)) = Option.default 0 init

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

(*
let add_var ({ var_map; current_scope; stack_index } as context) id =
  let var_map' = Map.add id (Stack stack_index) var_map in
  let current_scope' = Set.add id current_scope in
  { context with var_map=var_map';
                 current_scope=current_scope';
                 stack_index=stack_index - 4
  }
*)

(* TODO: prob doesn't belong here since gen.ml uses it *)
let get_const = function
  | None -> None
  | Some (Ast.Const c) ->
     begin
       match c with
       | Ast.Int i -> Some i
       (* TODO: better error handling here! *)
       | _ -> failwith "non-int initializers not implemented"
     end
  | _ -> failwith "non-constant initializer"

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

let add_static_local_var ({ var_map; var_decl_map; current_scope; } as context) id init =
  let label = Util.unique_id id in
  let init' = get_const init in
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
(*
let add_var
      ( { var_map; var_decl_map; current_scope; stack_index } as context)
      Ast.({ var_name=ID id; init; storage_class })
      linkg lbl =
  let get_const = function
    | None -> None
    | Some (Ast.Const c) ->
       begin
       match c with
       | Ast.Int i -> Some i
       (* TODO: better error handling here! *)
       | _ -> failwith "non-int initializers not implemented"
       end
    | _ -> failwith "non-constant initializer"
  in
  let stack_index', addr =
    match storage_class with
    | Ast.Nothing -> stack_index - 4, Stack stack_index
    | _ -> stack_index, Heap lbl
  in
  let var_decls' = Map.add id (HeapDecl (linkg, get_const init)) var_decl_map in
  { context with var_map=Map.add id addr var_map;
                 var_decl_map=var_decls';
                 current_scope=Set.add id current_scope;
                 stack_index=stack_index'
  }
*)
let var_lookup err_handler { var_map } var =
  try
    Map.find var var_map
  with
  | Not_found -> err_handler (Printf.sprintf "undeclared variable %s" var)

let opt_var_decl_lookup { var_decl_map } Ast.({ var_name=ID id }) =
  try
    Some (Map.find id var_decl_map)
  with
    | Not_found -> None
