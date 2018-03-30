(* Data types representing an abstract syntax tree *)
type const =
  | Int of int
  | Char of char
  | String of string

type type_def =
  | IntType
  | CharType

type binop =
  | Add
  | Sub
  | Mult
  | Div
  | Mod
  | Lt
  | Gt
  | Le
  | Ge
  | Neq
  | Eq
  | And
  | Or
  | BitAnd
  | BitOr
  | Xor
  | ShiftL
  | ShiftR

type assign_op =
  | Equals (* = *)

type unop = Negate | Pos | Complement | Not

type id = ID of string

type exp =
  | Const of const
  | Var of id
  | UnOp of unop * exp
  | BinOp of binop * exp * exp
  | TernOp of exp * exp * exp
  | Assign of assign_op * id * exp
  | FunCall of id * exp list

type declaration =
  { var_type: type_def;
    var_name: id;
    init: exp option;
  }

type block_item =
  | Statement of statement
  | Decl of declaration

and block = block_item list

and statement =
  | Block of block
  | If of {cond: exp; if_body: statement; else_body: statement option}
  | Exp of exp option
  | For of {init: exp option; cond: exp; post: exp option; body: statement}
  | ForDecl of {init: declaration; cond: exp; post: exp option; body: statement}
  | While of {cond: exp; body: statement}
  | DoWhile of {body: statement; cond: exp}
  | ReturnVal of exp (* should we add a return_exp instead? *)
  | Break
  | Continue

type fun_param = Param of type_def * id

type fun_decl =
  FunDecl of { fun_type: type_def;
               name: id;
               params: fun_param list;
               body: block;
             }

type prog = Prog of fun_decl list
