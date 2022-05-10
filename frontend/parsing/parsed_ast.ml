(* Specifies the structure of the parsed AST *)
open Ast_types

(** This is a complete statement.  Each Fortissimo program
    is comprised of complete statements*)
type compstmt =
  | Stmt        of statement
  | StEx        of statement * expression

(** Segments of code are separated via statements, not
    semicolons or whitespace
    Statements do not return values like expressions*)
and (*type*) statement =
  | Puts        of location * expression
  | StateIf     of location * statement * expression
  | StateWhile  of location * statement * expression
  | Expression  of expression

(** Returns a value*)
and (*type*) expression =
  | RetCArgsExp of location * call_args
  | OptCommand  of location * un_op option * command
  | Arg         of arg

and (*type*) call =
  | Task        of task
  | Command     of command

and (*type*) command =
  | OpCArgsComm of location * operation * call_args
  | PrimOpCArgs of location * primary * operation * call_args

and (*type*) task =
  | Operation   of location * operation
  | OpCArgsTask of location * operation * call_args

and (*type*) arg =
  | LequalsR    of location * lhs * rhs
  | OpResults   of op_results
  | OpDecs      of op_declarations
  | OpAssigns   of op_assignments
  | FuncCall    of function_call
  | LitArg      of literal
  | UnOp        of location * un_op * arg
  | BinOp       of location * bin_op * arg * arg
  | ParenArg    of location * arg
  | Primary     of primary

and (*type*) primary =
  | ParenCompSt of location * compstmt
  | LitPri      of literal
  | VariablePri of variable
  | PriArgs     of location * primary * args
  | Return      of location
  | RetCArgsPri of location * call_args
  | Task        of task  
  | IfPri       of if_statement
  | WhilePri    of location * expression * compstmt list
  | ForEver     of location * compstmt list
  | ForIn       of location * block_var * expression * compstmt list
  | ClassDef    of location * class_definition_header * class_definition_body
  | FunctionDef of location * function_definition

(** Need to modify to match OCaml's match-with *)
type when_args = Mrhs of mrhs

type block_var =
  | LhsBlock    of lhs
  | ForParamBlk of for_param
  (** | MlhsBlock of mlhs*)

(** Add type mlhs and type mlhs_item *)

type lhs =
  | VariableLhs of variable
  | PriArgs     of location * primary * arg list
  | PriIdent    of location * primary * identifier

type mrhs =
  | ArgsMrhs    of location * args
  | CommMrhs    of location * command

type args = arg list

(* Add type arg_list *)

type singleton =
  | SingleVar   of location * variable
  | SingleExp   of location * expression

(* Add type assocs and type assoc *)

type variable =
  | IdVar       of location * identifier
  | NilVar      of location * string
  | SelfVar     of location * string

type literal = 
  | Lit_bool    of string 
  | Lit_i32     of string (** 32 bit Integer literals such as [-1] [0] [10000] *)
  | Lit_i64     of string (** 64 bit Integer literals *)
  | Lit_f64     of string (** 64 bit Floating point numbers such as [-0.1111111111111] [10000.09876] *)
  | Lit_char    of char   (** Character such as ['f'] *)
  | Lit_str     of string * Location.t * string option

type class_definition_header = ClassDH of location * identifier * identifier option

type class_definition_body = ClassDB of location * class_variables option * function_definition list

type class_variables = ClassVar of location * (access_level * op_declarations) list

type access_level =
  | PublicAL    of string
  | PrivateAL   of string
  | ProtectedAL of string

type function_definition = FuncDef of location * function_definition_header * function_definition_body

type function_definition_body =
  | StmtFDB     of location * statement
  | StmtListFDB of location * compstmt list

type function_definition_header = FuncDH of location * access_level option * operation * dec_param list (* Replace with arg_list option *)

type function_call = FuncCall of location * operation * function_call_param list

type function_param =
  | NamedFuncPar of function_named_param
  | ResultsFPar  of op_results

type function_named_param = FuncNP of location * identifier * op_results

type else_expression = ElseExp of location * statement_body

type if_elsif_expression =
  | CondStBod     of location * cond_expression * statement_body
  | CondStBodElse of location * cond_expression * statement_body * else_expression
  | CondStBodElif of location * cond_expression * statement_body * if_elsif_expression

(* Add try, catch, and ensure *)

type progression =
  | IntProg       of location * string * string
(*| UIntProg      of location * string * string *)
  | CharProg      of location * char * char

type cond_expression =
(*| COrC          of location * comparison * cond_expression *)
(*| CXorC         of location * comparison * cond_expression *)
(*| CAndC         of location * comparison * cond_expression *)
  | CLOrC         of location * comparison * cond_expression
(*| CLXorC        of location * comparison * cond_expression *)
  | CLAndC        of location * comparison * cond_expression

type comparison = 
  | CompVarsLTGT  of location * comp_var * comp_var
  | CompVarsISNOT of location * comp_var * comp_var

type comp_var =
  | OpResCV       of op_results
  | ArraySelCV    of array_selector
  | IdCV          of identifier

type statement_body =
  | StmtListSB    of location * compstmt list
  | StmtSB        of location * compstmt

type op_results =
  | IntResults    of int_results
(*| UIntResults   of u_int_results *)
  | FloatResults  of float_results
  | CharResult    of char_result
  | StringResult  of string_result

type int_results = 
  | I32Result     of i32_result
  | I64Result     of i64_result

type i32_result =
  | I32Exp        of location * i32_result * i32_result
  | I32MulDivMod  of location * i32_result * i32_result
  | I32AddSub     of location * i32_result * i32_result
  | I32Paren      of location * i32_result
  | I32           of i32

type i64_result =
  | I64Exp        of location * i64_result * i64_result
  | I64MulDivMod  of location * i64_result * i64_result
  | I64AddSub     of location * i64_result * i64_result
  | I64Paren      of location * i64_result
  | I64           of i64

(* Add u_int_results *)

type float_results =
  | F64Result     of f64_result

type f64_result =
  | F64Exp        of location * f64_result * f64_result
  | F64MulDivMod  of location * f64_result * f64_result
  | F64AddSub     of location * f64_result * f64_result
  | F64Paren      of location * f64_result
  | F64           of f64

(* May have to modify *)
type char_result = Char_Def of char_def

type string_result =
  | StrAddStr     of location * string_result * string_result
  | IntMulStr     of location * int_results * string_result
  | StrMulInt     of location * string_result * int_results
(*| IntMulStr     of location * int_results * string_result *)
(*| StrMulInt     of location * string_result * int_results *)
  | Str_Def       of str_def

type array_list_elements =
  | SpResALE      of op_results
  | SpResALEList  of op_results list

type op_declarations =
(*| EnumDef       of enum_definition *)
(*| StructUnionDf of struct_union_definition *)
  | ArrayDecl     of array_declaration
  | ListDecl      of list_declaration
  | BoolDecl      of bool_declaration
  | IntDecls      of int_declarations
(*| UIntDecls     of u_int_declarations *)  
  | FloatDecls    of float_declarations
  | CharDecl      of char_declaration
  | StringDecl    of string_declaration
  | ArgDecl       of arg_declaration

(* Add enum, struct, and union def interfaces *)

type array_declaration = ArrayDeclaration of location * identifier * dec_param * array_list_elements option

type list_declaration = ListDeclaration of location * identifier * dec_param * array_list_elements option

type bool_declaration = BoolDeclarations of location * identifier * dec_param * bool_ops

type int_declarations = IntDeclarations of location * identifier * dec_param * int_results

(*type u_int_declarations = UIntDeclaration of location * identifier * dec_param * u_int_results *)

type float_declarations = FloatDeclarations of location * identifier * dec_param * float_results

type char_declaration = CharDeclarations of location * identifier * dec_param * char_result

type string_declaration = StringDeclaration of location * identifier * dec_param * string_result

type arg_declaration = ArgDeclation of location * identifier * dec_param * arg

type array_list_selector = ArrListSelector of location * identifier * string

type op_assignments =
  | ListAssn      of list_assignment
  | BoolAssn      of bool_assignment
  | IntAssns      of int_assignments
(*| UIntAssns     of u_int_assignments *)  
  | FloatAssns    of float_assignments
  | CharAssns     of char_assignment
  | StringAssn    of string_assignment
  | ArgAssn       of arg_assignment

type list_assignment = ListAssignment of location * array_list_selector * op_results

type bool_assignment = BoolAssignment of location * identifier * bool

type int_assignments = IntAssignments of location * identifier * int_results

(* type u_int_assignments = UIntAssignments of location * identifier * u_int_results *)

type float_assignments = FloatAssignments of location * identifier * float_results

type char_assignment = CharAssignment of location * identifier * char_result

type string_assignment = StringAssignment of location * identifier * string_result

type arg_assignment = ArgAssignment of location * identifier * arg_result

(* Referencing Ast_types *)
type identifier = Identifier of Id_name.t | ObjField of Id_name.t * Field_name.t

type operation =  Operation of Id_name.t

type for_param = 
  | ForParamInt   of location * identifier * string option
(*| ForParamU     of location * identifier * string option *)
  | ForParamChar  of location * identifier * char option

type dec_param =
  | ImmutDecParam of location * type_specifier
  | MutDecParam   of location * type_group
  | MutPreset     of location * identifier

type type_group =
  | TypeSpecifier of location * type_specifier
  | TypeGroup     of location * type_specifier list