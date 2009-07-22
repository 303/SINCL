(* 定数 *)

type int_cst = {
    prec        : int;      (* ビット数 *)
    is_unsigned : bool;
    value       : Int64.t;  (* めんどいので全部64ビットで保持 *)
}

type real_kind =
    | Real_Float
    | Real_Double
    | Real_LongDouble

type real_cst = {
    real_kind : real_kind;
    repr      : string;   (* めんどいので文字列 *)
}


(* 識別子 *)

type ident_kind =
    | Ident_Unknown
    | Ident_Struct
    | Ident_Union
    | Ident_Enum
    | Ident_EnumConst
    | Ident_Field       (* struct/unionのフィールド名 *)
    | Ident_TypedefName
    | Ident_Variable
    | Ident_GlobalVariable
    | Ident_ParameterVariable
    | Ident_FunctionPointer
    | Ident_FunctionName
    | Ident_Label

let ident_kind_to_string = function
    | Ident_Unknown           -> "<identifier kind unknown>"
    | Ident_Struct            -> "struct tag name"
    | Ident_Union             -> "union tag name"
    | Ident_Enum              -> "enum tag name"
    | Ident_EnumConst         -> "enumeration constant name"
    | Ident_Field             -> "field name of composite type"
    | Ident_TypedefName       -> "type name"
    | Ident_Variable          -> "variable"
    | Ident_GlobalVariable    -> "global variable"
    | Ident_ParameterVariable -> "parameter variable"
    | Ident_FunctionPointer   -> "function pointer"
    | Ident_FunctionName      -> "function name"
    | Ident_Label             -> "label name"


type storage =
    | Stor_NotSpecified
    | Stor_Static
    | Stor_Extern

(* Type Qualifier用ビットフラグ *)
let type_qual_const    = 1
let type_qual_volatile = 1 lsl 1
let type_qual_restrict = 1 lsl 2


type unary_operator =
    | Plus | Minus | Not | BitNot | AddressOf | IndirectRef

type incr_operator = Inc | Dec

type binary_operator =
    | Add | Sub | Mul | Div | Mod | And | Xor | Or | Lshift | Rshift
    | Padd | Psub | Pdiff | Seq | SeqAnd | SeqOr
    | Lt | Gt | Le | Ge | Eq | Ne

type ident = {
    id_name     : string;
    id_space_id : int;     (* 名前空間ID *)
    id_key      : int;           (* 識別子ID *)
    mutable id_kind : ident_kind;
    mutable id_type : itype;
}
and constant =
    | Const_Int of  int_cst
    | Const_Real of real_cst   (* float or double *)
    | Const_Enum of ident
    | Const_String of string 
and itype_desc =
    | VoidT
    | IntT of int * bool    (* ビット数, unsignedかどうか *)
    | RealT of real_kind
    | PointerT of itype
    | ArrayT of itype * (expression option)
    | FunctionT of itype * declaration list    
    | CompositeT of ident * ((declaration list) option) ref
and expression_desc =
    | NilE
    | VarE of ident
    | ConstE of constant
    | UnaryE of unary_operator * expression
    | PreE of incr_operator * expression
    | PostE of incr_operator * expression
    | BinaryE of binary_operator * expression * expression
    | AssignE of binary_operator option * expression * expression
    | ArrayRefE of expression * expression
    | FieldRefE of expression * ident
    | CallE of expression * expression list
    | TriE of expression * expression * expression
    | CastE of itype * expression
    | SizeofE of itype
    | ArrayInitE of expression list
and statement_desc =
    | DeclS of declaration list
    | ExprS of expression
    | LabelS of ident * statement
    | CaseS of expression * statement
    | DefaultS of statement
    | BlockS of statement list
    | CondS of expression * statement * statement
    | SwitchS of expression * statement
    | WhileS of expression * statement
    | DoS of expression * statement
    | ForS of statement * expression * expression * statement
    | GotoS of ident
    | ContinueS
    | BreakS
    | ReturnS of expression
and declaration_desc =
    | TypedefD of itype * ident
    | VarD of storage * itype * ident * expression option
    | ConstD of ident * expression option
    | FieldD of itype * ident 
    | ParamD of itype * ident option
    | EllipsisD (* 可変長引数の"..." *)
    | CompositeD of ident
and toplevel_desc =
    | FunDecl of storage * itype * ident * statement 
    | ExtDecl of declaration
    | Directive of string 
and itype = {
    ity_desc : itype_desc;
    ity_qual : int;
}
and expression = {
    exp_desc : expression_desc;
    exp_loc  : Srcloc.t;
    mutable exp_type : itype;
} 
and statement = {
    stmt_desc : statement_desc;
    stmt_loc  : Srcloc.t;
}
and declaration = {
    decl_desc : declaration_desc;
    decl_loc  : Srcloc.t;
}
and toplevel = {
    top_desc : toplevel_desc;
    top_loc  : Srcloc.t;
}

let is_nil_stmt stmt = match stmt.stmt_desc with
    | ExprS e when e.exp_desc = NilE    -> true
    | _ -> false

let make_char_cst c =
    Const_Int {
        prec = 8;
        is_unsigned = true; (* charはsignedだったはず *)
        value = Int64.of_int (Char.code c);
    }
let make_int32_cst unsigned value =
    Const_Int {
        prec = 32;
        is_unsigned = unsigned;
        value = value;
    }

let make_int64_cst unsigned value =
    Const_Int {
        prec = 64;
        is_unsigned = unsigned;
        value = value;
    }

let void_type = { ity_desc = VoidT; ity_qual = 0 }
let int_type  = { ity_desc = IntT(32, false); ity_qual = 0 }

let dummy_ident = {
    id_name     = "";
    id_space_id = 0;
    id_key      = 0;
    id_kind     = Ident_Union;
    id_type     = void_type;
}

let ident_id_gen = ref 0

let make_identifier kind ty name spid = 
    incr ident_id_gen;
    {
        id_name     = name;
        id_space_id = spid;
        id_key      = !ident_id_gen - 1;
        id_kind     = kind;
        id_type     = ty;
    }

let make_itype desc = {
    ity_desc = desc;
    ity_qual = 0;
}
