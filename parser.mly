%{
open Tree
open Type
open Lexing

let dbg msg = Printf.printf "%s\n" msg

let mktop desc = {
    top_desc = desc;
    top_loc = Srcloc.symbol_rloc();
}
let mkE desc = {
    exp_desc = desc;
    exp_type = void_type;
    exp_loc = Srcloc.symbol_rloc();
}
let mkS desc = { stmt_desc = desc; stmt_loc = Srcloc.symbol_rloc() }
let mkD desc = { decl_desc = desc; decl_loc = Srcloc.symbol_rloc() }

let is_global = ref true

%}

%token <Tree.ident> Tidentifier Ttypedef_name Tenum_const
%token <Tree.constant> Tcharacter_cst Tinteger_cst
Tfloating_cst  Tstring_literal
%token <string> Tdirective
%token Tsizeof
%token Tarrow Tplusplus Tminusminus Tlshift Trshift Tle Tge Teq Tne
%token Tseqand Tseqor Tstarasgn Tdivasgn Tmodasgn Taddasgn
%token Tsubasgn Tlshiftasgn Trshiftasgn Tampasgn
%token Txorasgn Torasgn
%token Ttypedef Textern Tstatic Tregister Trestrict
%token Tchar Tshort Tint Tlong Tsigned Tunsigned Tfloat Tdouble Tconst Tvolatile Tvoid
%token Tstruct Tunion Tenum Tellipsis
%token Tcase Tdefault Tif Telse Tswitch Twhile Tdo Tfor Tgoto Tcontinue Tbreak Treturn 
%token Tcomma Tasgn Tquestion Tcolon Tor Txor Tamp Ttilde Tbang
%token Tlt Tgt Tplus Tminus Tstar Tdiv Tmod 
%token Tlbrace Trbrace Tlbracket Trbracket Tlparen Trparen Tdot Tsemi
%token Teof

%left  Tcomma
%right Tasgn Taddasgn Tsubasgn Tmulasgn Tdivasgn Tmodasgn Tlshiftasgn
Trshiftasgn Tandasgn Txorasgn Torasgn
%right Tquestion Tcolon
%left  Tseqor
%left  Tseqand
%left  Tor
%left  Txor
%left  Tamp
%left  Teq Tne
%left  Tlt Tgt Tle Tge
%left  Tlshift Trshift
%left  Tplus Tsub
%left  Tstar Tdiv Tmod
%right prec_cast
%right prec_unary prec_preop Tsizeof
%left  Tlbracket Trbracket Tlparen Trparen prec_postop Tdot Tarrow
%left  prec_array_ref prec_function_call

%type <Tree.toplevel list> translation_unit
%start translation_unit

%%
nsenter:
    |   { Idtable.enter_new_namespace() }
nsleave:
    |   { Idtable.leave_namespace() }
nsreenter:
    |   { Idtable.reenter_namespace() }

translation_unit:
    | external_declaration_list Teof    { $1 }

external_declaration_list: 
    |                       { [] }
    | external_declaration_list external_declaration { $1 @ $2 }

external_declaration:
    | Tdirective
    { [mktop (Directive $1)] }
    | function_definition       { $1 }
    | declaration
    { List.map (fun decl -> mktop (ExtDecl decl) ) $1 }

/* Declaration */

declaration_specifiers:
    | storage_class_specifier itype { ($1, $2) }

declaration:
    | Ttypedef itype declarator Tsemi
    {
        let ty = construct_type $2 $3.decl_spec in
        let id = Idtable.register_normal_ident (Srcloc.rhs_loc 3) Ident_TypedefName
        ty $3.decl_id.id_name in
        [mkD (TypedefD(ty, id))]
    }
    | composite_type_decl   { [$1] } 
    | declaration_specifiers init_declarator_list Tsemi
    {
        let kind = if !is_global then Ident_GlobalVariable else Ident_Variable in
        List.map (fun decl ->
            let ty = construct_type (snd $1)  decl.decl_spec in
            let id = Idtable.register_normal_ident (Srcloc.rhs_loc 2) kind
            ty decl.decl_id.id_name in
            mkD (VarD(fst $1, ty, id, decl.decl_init))
        ) (List.rev $2)
    }

init_declarator_list:   /* reversed order */
    | init_declarator   { [$1] }
    | init_declarator_list Tcomma init_declarator  { $3 :: $1 }

init_declarator:
    | declarator        { $1 }
    | declarator Tasgn init { { $1 with decl_init = Some $3 } }

init:
   | assignment_expression  { $1 }
   | Tlbrace init_list Trbrace   
   { mkE (ArrayInitE (List.rev $2)) }
   | Tlbrace init_list Tcomma Trbrace    
   { mkE (ArrayInitE (List.rev $2)) }

/*
 * 下の様な古い形式はサポートしない．
 * void f(x, y)
 *  int x;
 *  int y;
 * {
 *    ..
 * }
 */
function_definition:
    | declaration_specifiers function_declarator 
    Tlbrace nsreenter blockitem_list_opt nsleave Trbrace
    {
        let ty = construct_type (snd $1) $2.decl_spec in
        Idtable.clear_label_table();    (* ラベルは関数単位で一意 *)
        [mktop (FunDecl(fst $1, ty, $2.decl_id, mkS (BlockS (List.rev $5))))]
    }

function_declarator:
    | pointer function_direct_declarator { add_declspec $2 $1 }
    | function_direct_declarator         { $1 }

function_direct_declarator:
    | direct_declarator Tlparen nsenter parameter_type_list nsleave Trparen
    { add_declspec $1 [Decl_Function (List.rev $4)] }
    | direct_declarator Tlparen Trparen
    { add_declspec $1 [Decl_Function []] }

declarator:
    | pointer direct_declarator { add_declspec $2 $1 }
    | direct_declarator         { $1 }

pointer:
    | Tstar typequalifiers          { [Decl_PointerTo $2] }
    | Tstar typequalifiers pointer  { $3 @ [Decl_PointerTo $2] }

direct_declarator:
    | Tidentifier       { { decl_id = $1; decl_spec = []; decl_init = None} }
    | Tlparen declarator Trparen    { $2 }
    | direct_declarator Tlbracket Trbracket
    { add_declspec $1 [Decl_ArrayOf(None, 0)] }
    | direct_declarator Tlbracket typequalifiers assignment_expression Trbracket
    { add_declspec $1 [Decl_ArrayOf(Some $4, $3)] }
    | direct_declarator Tlparen Trparen
    { add_declspec $1 [Decl_Function []] }
    | direct_declarator Tlparen nsenter parameter_type_list nsleave Trparen
    { add_declspec $1 [Decl_Function (List.rev $4)] }
    /* 他にもいろいろあるけど後回し */

abstract_declarator_opt:
    |                       { [] }
    | abstract_declarator   { $1 }

abstract_declarator:
    | pointer                               { $1 }
    | direct_abstract_declarator            { $1 }
    | pointer direct_abstract_declarator    { $2 @ $1 }

direct_abstract_declarator:
    | Tlparen abstract_declarator Trparen   { $2 } 
    | Tlbracket assignment_expression_opt Trbracket
    { [Decl_ArrayOf($2, 0)] }
    | direct_abstract_declarator Tlbracket assignment_expression_opt Trbracket
    { $1 @ [Decl_ArrayOf($3, 0)] }
    | Tlparen Trparen
    { [Decl_Function []] }
    | Tlparen nsenter parameter_type_list nsleave Trparen
    { [Decl_Function (List.rev $3)] }
    | direct_abstract_declarator Tlparen Trparen
    { $1 @ [Decl_Function []] }
    | direct_abstract_declarator Tlparen nsenter parameter_type_list nsleave Trparen
    { $1 @ [Decl_Function (List.rev $4)] }

/* 可変長引数は後回し */
parameter_type_list: /* reverse order */
    | parameter_list    { $1 }
    | parameter_list Tcomma Tellipsis
    { mkD EllipsisD :: $1 }

parameter_list: /* reverse order */
    | parameter_declaration                         { [$1] }
    | parameter_list Tcomma parameter_declaration   { $3 :: $1 }

parameter_declaration:
    | itype declarator
    {
        let ty = construct_type $1 $2.decl_spec in
        mkD (ParamD(ty, Some $2.decl_id))
    }
    | itype abstract_declarator_opt
    {
        let ty = construct_type $1 $2 in
        mkD (ParamD(ty, None))
    }

/* designatorは後回し */
init_list:   /* reversed order */
    | init   { [$1] }
    | init_list Tcomma init   { $3 :: $1 }

itype:
    | typequalifiers itype_simple   
    { { $2 with ity_qual = $1 } }
    | itype_simple   { $1 }

itypename:
    | itype abstract_declarator_opt { construct_type $1 $2 }

/* conflictを無くす為の力技 */
inttype:
    | Tchar            { make_itype (IntT(8, false)) }
    | Tshort           { make_itype (IntT(16, false)) }
    | Tshort Tint      { make_itype (IntT(16, false)) }
    | Tint             { make_itype (IntT(32, false)) }
    | Tlong            { make_itype (IntT(32, false)) }
    | Tlong Tint       { make_itype (IntT(32, false)) }
    | Tlong Tlong      { make_itype (IntT(64, false)) }
    | Tlong Tlong Tint { make_itype (IntT(64, false)) }
    | Tunsigned Tchar            { make_itype (IntT(8, true)) }
    | Tunsigned Tshort           { make_itype (IntT(16, true)) }
    | Tunsigned Tshort Tint      { make_itype (IntT(16, true)) }
    | Tunsigned Tint             { make_itype (IntT(32, true)) }
    | Tunsigned Tlong            { make_itype (IntT(32, true)) }
    | Tunsigned Tlong Tint       { make_itype (IntT(32, true)) }
    | Tunsigned Tlong Tlong      { make_itype (IntT(64, true)) }
    | Tunsigned Tlong Tlong Tint { make_itype (IntT(64, true)) }

realtype:
    | Tfloat        { make_itype (RealT Real_Float) }
    | Tdouble       { make_itype (RealT Real_Double) }
    | Tlong Tdouble { make_itype (RealT Real_LongDouble) }

composite_type:
    | Tstruct sue_identifier    { $2.id_type }
    | Tunion  sue_identifier    { $2.id_type }
    | Tenum   sue_identifier    { $2.id_type }

itype_simple:
    | Tvoid             { void_type }
    | inttype           { $1 }
    | realtype          { $1 }
    | composite_type    { $1 }
    | Ttypedef_name     { $1.id_type }

typequalifier:
    | Tconst    { type_qual_const }
    | Tvolatile { type_qual_volatile }
    | Trestrict { type_qual_volatile }

typequalifiers:
    |                               { 0 }
    | typequalifiers typequalifier  { $1 lor $2 } 

/* auto/registerは無くす */
storage_class_specifier:
    |           { Stor_NotSpecified }
    | Tstatic   { Stor_Static }
    | Textern   { Stor_Extern }


sue_identifier:
    | Tidentifier   { $1 }
    | Ttypedef_name { $1 }
    | Tenum_const   { $1 }

composite_type_decl:
    | Tenum sue_identifier Tlbrace enumerator_list Trbrace Tsemi
    {
        let loc = Srcloc.rhs_loc 2 in
        let id = Idtable.get_sue_ident loc Ident_Enum $2.id_name in
        Type.construct_sue_type id (List.rev $4);
        mkD (CompositeD id)
    }
    | Tenum sue_identifier Tlbrace enumerator_list Tcomma Trbrace Tsemi
    {
        let loc = Srcloc.rhs_loc 2 in
        let id = Idtable.get_sue_ident loc Ident_Enum $2.id_name in
        Type.construct_sue_type id (List.rev $4);
        mkD (CompositeD id)
    }
    | Tstruct sue_identifier Tlbrace nsenter struct_declaration_list_opt nsleave Trbrace Tsemi
    {
        let loc = Srcloc.rhs_loc 2 in
        let id = Idtable.get_sue_ident loc Ident_Struct $2.id_name in
        Type.construct_sue_type id $5;
        mkD (CompositeD id)
    }
    | Tunion sue_identifier Tlbrace nsenter struct_declaration_list_opt nsleave Trbrace Tsemi
    {
        let loc = Srcloc.rhs_loc 2 in
        let id = Idtable.get_sue_ident loc Ident_Struct $2.id_name in
        Type.construct_sue_type id $5;
        mkD (CompositeD id)
    }


enumerator_list:    /* reversed order */
    | enumerator                        { [$1] }
    | enumerator_list Tcomma enumerator { $3 :: $1 }

enumerator:
    | Tidentifier
    {
        let id = Idtable.register_normal_ident (Srcloc.rhs_loc 1)
        Ident_EnumConst int_type $1.id_name in
        mkD (ConstD(id, None))
    }
    | Tidentifier Teq constant_expression
    {
        let id = Idtable.register_normal_ident (Srcloc.rhs_loc 1)
        Ident_EnumConst int_type $1.id_name in
        mkD (ConstD(id, Some $3))
    }


struct_declaration_list_opt:    /* normal order */
    |                                                { [] }
    | struct_declaration_list_opt struct_declaration { $1 @ $2 }

struct_declaration:
    | itype struct_declarator_list Tsemi
    {
        List.map (fun decl ->
            let ty = Type.construct_type $1 decl.decl_spec in
            mkD (FieldD(ty, decl.decl_id))
        ) $2
    }

struct_declarator_list: /* reversed order */
    | struct_declarator                                 { [$1] }
    | struct_declarator_list Tcomma struct_declarator   { $3 :: $1 }

/* bit-fieldは後回し */
struct_declarator:
    | declarator    { $1 }


/* Statement */

statement:
    | labeled_statement    { $1 }
    | compound_statement   { $1 }
    | expression_statement { $1 }
    | selection_statement  { $1 }
    | iteration_statement  { $1 }
    | jump_statement       { $1 }

labeled_statement:
    | label Tcolon statement
    { mkS (LabelS($1, $3)) }
    | Tcase constant_expression Tcolon statement
    { mkS (CaseS($2, $4)) }
    | Tdefault Tcolon statement
    { mkS (DefaultS($3)) }

/* lexerではidtable_normalの探索を行うので,idtable_labelの検索をしなおす */
label:
    | Tidentifier   { Idtable.get_label_ient $1.id_name }
    | Ttypedef_name { Idtable.get_label_ient $1.id_name }
    | Tenum_const   { Idtable.get_label_ient $1.id_name }

compound_statement:
    | Tlbrace nsenter blockitem_list_opt nsleave Trbrace
    { mkS (BlockS (List.rev $3)) }

blockitem_list_opt: /* reversed order */
    |                                { [] }
    | blockitem_list_opt declaration { mkS (DeclS $2) :: $1 }
    | blockitem_list_opt statement   { $2 :: $1 }

expression_statement:
    | expression_opt Tsemi   { mkS (ExprS $1) }

expression_opt:
    |            { mkE NilE }
    | expression { $1 }

selection_statement:
    | Tif Tlparen expression Trparen statement
    { mkS (CondS($3, $5, mkS (ExprS (mkE NilE)))) }
    | Tif Tlparen expression Trparen statement Telse statement
    { mkS (CondS($3, $5, $7)) }
    | Tswitch Tlparen expression Trparen statement
    { mkS (SwitchS($3, $5)) }

iteration_statement:
    | Twhile Tlparen expression Trparen statement
    { mkS (WhileS($3, $5)) }
    | Tdo statement Twhile Tlparen expression Trparen Tsemi
    { mkS (DoS($5, $2)) }
    | Tfor Tlparen expression_opt Tsemi expression_opt Tsemi expression_opt
    Trparen statement
    { mkS (ForS(mkS(ExprS $3), $5, $7, $9)) }
    /* 後回し
    | Tfor Tlparen declaration expression_opt Tsemi expression_opt Trparen statement
    { mkS (ForS(mkS(DeclS $3), $4, $6, $8)) }
    */

jump_statement:
    | Tgoto label Tsemi 
    { mkS (GotoS $2) }
    | Tcontinue Tsemi
    { mkS ContinueS }
    | Tbreak Tsemi
    { mkS BreakS }
    | Treturn expression_opt Tsemi
    { mkS (ReturnS $2) }

/* Expression */

expression:
    | assignment_expression { $1 }
    | expression Tcomma assignment_expression
    { mkE (BinaryE(Seq, $1, $3)) }

assignment_expression:
    | conditional_expression    { $1 }
    | unary_expression Tasgn expression
    { mkE (AssignE(None, $1, $3)) } 
    | unary_expression Tmulasgn expression
    { mkE (AssignE(Some Mul, $1, $3)) }
    | unary_expression Tdivasgn expression
    { mkE (AssignE(Some Div, $1, $3)) }
    | unary_expression Tmodasgn expression
    { mkE (AssignE(Some Mod, $1, $3)) }
    | unary_expression Taddasgn expression
    { mkE (AssignE(Some Add, $1, $3)) }
    | unary_expression Tsubasgn expression
    { mkE (AssignE(Some Sub, $1, $3)) }
    | unary_expression Tlshiftasgn expression
    { mkE (AssignE(Some Lshift, $1, $3)) }
    | unary_expression Trshiftasgn expression
    { mkE (AssignE(Some Rshift, $1, $3)) }
    | unary_expression Tandasgn expression
    { mkE (AssignE(Some And, $1, $3)) }
    | unary_expression Txorasgn expression
    { mkE (AssignE(Some Xor, $1, $3)) }
    | unary_expression Torasgn expression
    { mkE (AssignE(Some Or, $1, $3)) }

conditional_expression:
    | logical_or_expression { $1 }
    | logical_or_expression Tquestion expression Tcolon conditional_expression
    { mkE (TriE($1, $3, $5)) }

constant_expression:
    | conditional_expression    { $1 }

logical_or_expression:
    | logical_and_expression    { $1 }
    | logical_or_expression Tseqor logical_and_expression
    { mkE (BinaryE(SeqOr, $1, $3)) }

logical_and_expression:
    | inclusive_or_expression   { $1 }
    | logical_and_expression Tseqand inclusive_or_expression
    { mkE (BinaryE(SeqAnd, $1, $3)) }

inclusive_or_expression:
    | exclusive_or_expression   { $1 }
    | inclusive_or_expression Tor exclusive_or_expression
    { mkE (BinaryE(Or, $1, $3)) }

exclusive_or_expression:
    | and_expression    { $1 }
    | exclusive_or_expression Txor and_expression
    { mkE (BinaryE(Xor, $1, $3)) }

and_expression:
    | equality_expression   { $1 }
    | and_expression Tamp equality_expression
    { mkE (BinaryE(And, $1, $3)) }

equality_expression:
    | relation_expression   { $1 }
    | equality_expression Teq relation_expression
    { mkE (BinaryE(Eq, $1, $3)) }
    | equality_expression Tne relation_expression
    { mkE (BinaryE(Ne, $1, $3)) }

relation_expression:
    | shift_expression  { $1 }
    | relation_expression Tlt shift_expression
    { mkE (BinaryE(Lt, $1, $3)) }
    | relation_expression Tgt shift_expression
    { mkE (BinaryE(Lt, $1, $3)) }
    | relation_expression Tle shift_expression
    { mkE (BinaryE(Le, $1, $3)) }
    | relation_expression Tge shift_expression
    { mkE (BinaryE(Ge, $1, $3)) }

shift_expression:
    | additive_expression   { $1 }
    | shift_expression Tlshift additive_expression
    { mkE (BinaryE(Lshift, $1, $3)) }
    | shift_expression Trshift additive_expression
    { mkE (BinaryE(Rshift, $1, $3)) }

additive_expression:
    | multiplicative_expression { $1 }
    | additive_expression Tplus multiplicative_expression
    { mkE (BinaryE(Add, $1, $3)) }
    | additive_expression Tminus multiplicative_expression
    { mkE (BinaryE(Sub, $1, $3)) }

multiplicative_expression:
    | cast_expression   { $1 }
    | multiplicative_expression Tstar cast_expression
    { mkE (BinaryE(Mul, $1, $3)) }
    | multiplicative_expression Tdiv cast_expression
    { mkE (BinaryE(Div, $1, $3)) }
    | multiplicative_expression Tmod cast_expression
    { mkE (BinaryE(Mod, $1, $3)) }

cast_expression:
    | unary_expression  { $1 }
    | Tlparen itypename Trparen cast_expression %prec prec_cast
    { mkE (CastE($2, $4)) }

unary_expression:
    | postfix_expression    { $1 }
    | Tplusplus unary_expression %prec prec_preop
    { mkE (PreE(Inc, $2)) } 
    | Tminusminus unary_expression %prec prec_preop
    { mkE (PreE(Dec, $2)) }
    | Tplus cast_expression %prec prec_unary
    { mkE (UnaryE(Plus, $2)) }
    | Tminus cast_expression %prec prec_unary
    { mkE (UnaryE(Minus, $2)) }
    | Ttilde cast_expression %prec prec_unary
    { mkE (UnaryE(BitNot, $2)) }
    | Tbang cast_expression %prec prec_unary
    { mkE (UnaryE(Not, $2)) }
    | Tamp cast_expression %prec prec_unary
    { mkE (UnaryE(AddressOf, $2)) }
    | Tstar cast_expression %prec prec_unary
    { mkE (UnaryE(IndirectRef, $2)) }
    | Tsizeof unary_expression
    { mkE (SizeofE($2.exp_type)) }
    | Tsizeof Tlparen itypename Trparen
    { mkE (SizeofE $3) } 


postfix_expression:
    | primary_expression    { $1 }
    | postfix_expression Tlbracket expression Trbracket
    { mkE (ArrayRefE($1, $3)) }
    | postfix_expression Tlparen argument_expression_list_opt Trparen
    %prec prec_function_call
    { mkE (CallE($1, List.rev $3)) } 
    | postfix_expression Tdo Tidentifier
    {
        Idtable.check_normal_ident (Srcloc.rhs_loc 3) $3 Ident_Field;
        mkE (FieldRefE($1,$3))
    }
    | postfix_expression Tarrow Tidentifier
    {
        Idtable.check_normal_ident (Srcloc.rhs_loc 3) $3 Ident_Field;
        (* a->b is equivalent to ( *a ).b *)
        mkE (FieldRefE (mkE (UnaryE(IndirectRef, $1)), $3))
    }
    | postfix_expression Tplusplus 
    %prec prec_postop
    { mkE (PostE(Inc, $1)) }
    | postfix_expression Tminusminus 
    %prec prec_postop
    { mkE (PostE(Dec, $1)) }

primary_expression:
    | Tidentifier           { mkE (VarE $1) }
    | constant              { mkE (ConstE $1) }
    | Tlparen expression Trparen    { $2 }  

argument_expression_list_opt: /* reversed order */
    |                           { [] } 
    | argument_expression_list  { $1 }

argument_expression_list: /* reversed order */
    | assignment_expression { [$1] }
    | argument_expression_list Tcomma assignment_expression
    { $3 :: $1 }

assignment_expression_opt:
    |                       { None }
    | assignment_expression { Some $1 }

constant:
    | Tfloating_cst     { $1 }
    | Tinteger_cst      { $1 }
    | Tcharacter_cst    { $1 }
    | Tstring_literal   { $1 }
    | Tenum_const       { Tree.Const_Enum $1 }
