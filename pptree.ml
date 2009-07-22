(* pretty printing *)

(*
 * Cのコードを生成する．
 *
 * TODO:
 * 後でgcc等にかける訳だが、エラーメッセージなどのファイル名・行番号
 * が元のソースと同じになるようにディレクティブを挿入する．
 *)

open Srcloc
open Type
open Tree
open Lexing
open Format

(*
 * OCamlのFormatモジュールのインデント仕様はCのフォーマットには適していない
 * 使用ので，自前でインデントを管理する．
 * 大分汚くなってしまった．
 *)

let indent_width = ref 0

let incr_indent () = indent_width := !indent_width + 4
let decr_indent () = indent_width := !indent_width - 4

let indent off = String.make (!indent_width + off) ' '

let storage_str = function
    | Stor_NotSpecified -> ""
    | Stor_Static -> "static "
    | Stor_Extern -> "extern "

let suetag_str = function
    | Ident_Struct  -> "struct"
    | Ident_Union   -> "union"
    | Ident_Enum    -> "enum"
    | _ -> failwith "not reachable"

let typequal_str qual =
    String.concat "" [
        if (qual land type_qual_const) <> 0 then "const " else "";
        if (qual land type_qual_volatile) <> 0 then "volatile " else "";
        if (qual land type_qual_restrict) <> 0 then "restrict " else ""]

let unaryop_char = function
    | Plus        -> '+'
    | Minus       -> '-'
    | Not         -> '!'
    | BitNot      -> '~'
    | AddressOf   -> '&'
    | IndirectRef -> '*'

let incrop_str = function
    | Inc   -> "++"
    | Dec   -> "--"

let binaryop_str = function
    | Add    -> "+"
    | Sub    -> "-"
    | Mul    -> "*"
    | Div    -> "/"
    | Mod    -> "%"
    | And    -> "&"
    | Xor    -> "^"
    | Or     -> "|"
    | Lshift -> "<<"
    | Rshift -> ">>"
    | Padd   -> "+"
    | Psub   -> "-"
    | Pdiff  -> "-"
    | Seq    -> ","
    | SeqAnd -> "&&"
    | SeqOr  -> "||"
    | Lt     -> "<"
    | Gt     -> ">"
    | Le     -> "<="
    | Ge     -> ">="
    | Eq     -> "=="
    | Ne     -> "!="

(* 値の小さい方が優先順位大 *)

(* カンマ演算子とその他のカンマの区別が必要
 * f(a, (b,c))
 * とか
 * { a, (b,c), d}
 * などのようにカンマ演算子とカンマが同時に出たときは式に括弧が必要
 *)
let comma_priority = 16
let seq_priority   = 17
let binaryop_priority = function
    | Add    -> 5
    | Sub    -> 5
    | Mul    -> 4
    | Div    -> 4
    | Mod    -> 4
    | And    -> 9
    | Xor    -> 10
    | Or     -> 11
    | Lshift -> 6
    | Rshift -> 6
    | Padd   -> 5
    | Psub   -> 5
    | Pdiff  -> 5
    | Seq    -> seq_priority
    | SeqAnd -> 12
    | SeqOr  -> 13
    | Lt     -> 7
    | Gt     -> 7
    | Le     -> 7
    | Ge     -> 7
    | Eq     -> 8
    | Ne     -> 8

let expr_priority expr = match expr.exp_desc with
    | NilE            -> 0
    | VarE _          -> 0
    | ConstE  _       -> 0
    | UnaryE(_,_)     -> 2
    | PreE(_,_)       -> 2
    | PostE(_,_)      -> 1
    | BinaryE(op,_,_) -> binaryop_priority op
    | AssignE(_,_,_)  -> 15
    | ArrayRefE(_,_)  -> 1
    | FieldRefE(_,_)  -> 1
    | CallE(_,_)      -> 1
    | TriE(_,_,_)     -> 14
    | CastE(_,_)      -> 3
    | SizeofE _       -> 2
    | ArrayInitE _    -> comma_priority

let pp_identifier ppf id = fprintf ppf "%s" id.id_name

let pp_int ppf cst = 
    if cst.is_unsigned
        then fprintf ppf "%Lu" cst.value
        else fprintf ppf "%Ld" cst.value

let pp_constant ppf = function
    | Const_Int cst  -> pp_int ppf cst
    | Const_Real cst -> fprintf ppf "%s" cst.repr
    | Const_Enum id  -> pp_identifier ppf id
    | Const_String str -> fprintf ppf "\"%s\"" (String.escaped str)

let pp_type_desc ppf = function
    | VoidT         -> fprintf ppf "void"
    | IntT(bit,usgn) -> fprintf ppf "%s%s" 
        (if usgn then "unsigned " else "")
        (match bit with
            | 8     -> "char"
            | 16    -> "short"
            | 32    -> "int"
            | 64    -> "long long"
            | _     -> failwith "not reachable"
        )
    | RealT kind -> begin
        match kind with
            | Real_Float      -> fprintf ppf "float"
            | Real_Double     -> fprintf ppf "double"
            | Real_LongDouble -> fprintf ppf "@[long double@]"
        end
    | _ -> failwith "not reachable"

let pp_type ppf ty =
    fprintf ppf "@[%s%a@]" (typequal_str ty.ity_qual)
        pp_type_desc ty.ity_desc

let rec pp_expr ppf exp = 
    let pri = expr_priority exp in
    match exp.exp_desc with
    | NilE            -> fprintf ppf ""
    | VarE id         -> pp_identifier ppf id
    | ConstE cst      -> pp_constant ppf cst
    | UnaryE(op, e) -> fprintf ppf "@[%c%a@]" (unaryop_char op) 
        pp_expr_p (e, pri)
    | PreE(op, e)   -> fprintf ppf "@[%s%a@]" (incrop_str op) 
        pp_expr_p (e, pri)
    | PostE(op, e)  -> fprintf ppf "@[%a%s@]" 
        pp_expr_p (e, pri) (incrop_str op)
    | BinaryE(op, e1, e2) ->
            fprintf ppf "@[%a@;%s@;%a@]"
            pp_expr_p (e1, pri) (binaryop_str op)
            pp_expr_p (e2, pri)
    | AssignE(None, e1, e2) ->
            fprintf ppf "@[%a =@;%a@]"
            pp_expr_p (e1, pri) pp_expr_p (e2, pri)
    | AssignE(Some op, e1, e2) ->
            fprintf ppf "@[%a %s=@;%a@]"
            pp_expr_p (e1, pri) (binaryop_str op)
            pp_expr_p (e2, pri)
    | ArrayRefE(ary,idx) ->
            fprintf ppf "@[%a[%a]@]"
            pp_expr_p (ary, pri) pp_expr idx
    | FieldRefE(e,mem) ->
            fprintf ppf "@[%a.%a@]"
            pp_expr_p (e, pri) pp_identifier mem
    | CallE(func,args) ->
            fprintf ppf "@[%a(@[<hov>%a@])@]"
            pp_expr_p (func, pri) pp_expr_list args
    | TriE(e1,e2,e3) ->
            fprintf ppf "@[%a?%a:%a@]"
            pp_expr_p (e1, pri) pp_expr e2 pp_expr_p (e3, pri)
    | CastE(ty, e) ->
            fprintf ppf "@[(%a) %a@]"
            pp_abst_decllhs ty pp_expr_p (e, pri)
    | SizeofE ty ->
            fprintf ppf "@[sizeof(%a)@]" pp_abst_decllhs ty
    | ArrayInitE es ->
            fprintf ppf "@[{@[<hov>%a@]}@]"
            pp_expr_list es

(* もしeの優先順位が外側の式の優先順位より低かったら括弧をつける *)
and pp_expr_p ppf (e, outer_priority) =
    let priority = expr_priority e in
    if outer_priority < priority
        then fprintf ppf "(%a)" pp_expr e
        else fprintf ppf "%a" pp_expr e

and pp_expr_list ppf = function
    | []    -> fprintf ppf ""
    | [e]   -> pp_expr_p ppf (e, comma_priority)
    | e1::e2::rem ->
            fprintf ppf "%a,@;%a" pp_expr_p (e1, comma_priority)
            pp_expr_list (e2::rem)

and pp_declarator ppf (id, dspecs, need_paren) = match dspecs with
    | []    ->
        begin match id with
            | Some id   -> pp_identifier ppf id
            | None      -> ()
        end
    | d::rem -> match d with
        | Decl_PointerTo qual ->
        if need_paren 
            then fprintf ppf "(*%s%a)" (typequal_str qual)
                pp_declarator (id, rem, false)
            else fprintf ppf "*%s%a" (typequal_str qual)
                pp_declarator (id, rem, false)
        | Decl_ArrayOf(None,_) ->
                fprintf ppf "%a[]" pp_declarator (id, rem, true)
        | Decl_ArrayOf(Some len, qual) ->
                fprintf ppf "%a[%s%a]" pp_declarator (id, rem, true)
                    (typequal_str qual) pp_expr len 
        | Decl_Function [] ->
                fprintf ppf "%a()" pp_declarator (id, rem, true)
        | Decl_Function params ->
                fprintf ppf "%a(@[%a@])" pp_declarator (id, rem, true)
                    pp_param_list params

and pp_decllhs ppf (stor, ty, id) = 
    let base, dspecs = deconstruct_type ty in
    fprintf ppf "@[%s%a@;%a@]" (storage_str stor) pp_type base
        pp_declarator (Some id, dspecs, false)

and pp_abst_decllhs ppf ty =
    let base, dspecs = deconstruct_type ty in
    fprintf ppf "%a %a" pp_type base pp_declarator (None, dspecs, false)

and pp_param_list ppf = function
    | []    -> fprintf ppf ""
    | [d]   -> pp_decl ppf d
    | d1::d2::rem ->
            fprintf ppf "%a,@;%a" pp_decl d1 pp_param_list (d2::rem)
  
and pp_composite_decl ppf ty = match ty.ity_desc with
    | CompositeT(id, d) -> 
        let decls = match !d with
            | None  -> failwith "not reachable"
            | Some d -> d
        in 
        fprintf ppf "%s %a {@\n" (suetag_str id.id_kind) pp_identifier id;
        incr_indent();
        pp_decl_list ppf decls;
        decr_indent();
        fprintf ppf "}@."
    | _ -> failwith "not reachable"

and pp_decl ppf decl = match decl.decl_desc with
    | TypedefD(ty, id) ->
            fprintf ppf "@[typedef %a;@]" pp_decllhs (Stor_NotSpecified, ty, id)
    | VarD(stor, ty, id, None) ->
            fprintf ppf "@[%a;@]" pp_decllhs (stor, ty, id)
    | VarD(stor, ty, id, Some init) ->
            fprintf ppf "@[%a =@;%a;@]" pp_decllhs (stor, ty, id) pp_expr init
    | ConstD(id, None) ->
            fprintf ppf "%a," pp_identifier id
    | ConstD(id, Some value) ->
            fprintf ppf "@[%a = @;%a,@]" pp_identifier id pp_expr value
    | FieldD(ty,id) ->
            fprintf ppf "@[%a;@]" pp_decllhs (Stor_NotSpecified, ty, id)
    | ParamD(ty, None)    -> pp_abst_decllhs ppf ty
    | ParamD(ty, Some id) -> pp_decllhs ppf (Stor_NotSpecified, ty, id)
    | EllipsisD     -> fprintf ppf "..."
    | CompositeD id -> pp_composite_decl ppf id.id_type

and pp_decl_list ppf decls =
    List.iter (fun decl -> fprintf ppf "%s%a@." (indent 0) pp_decl decl) decls

let rec pp_stmt ppf stmt = match stmt.stmt_desc with
    | DeclS decls   -> fprintf ppf "%a" pp_decl_list decls
    | ExprS e       -> fprintf ppf "%s%a;@." (indent 0) pp_expr e
    | LabelS(lbl,stmt) ->
            fprintf ppf "%s%a:@\n%s%a@." (indent (-4)) pp_identifier lbl
            (indent 0) pp_stmt stmt
    | CaseS(v,stmt) ->
            fprintf ppf "%scase %a:@\n%s%a@." (indent (-4)) pp_expr v 
            (indent 0) pp_stmt stmt
    | DefaultS stmt ->
            fprintf ppf "%sdefault:%s%a@." (indent (-4)) (indent 0) pp_stmt stmt
    | BlockS stmts -> 
            fprintf ppf "%s{@\n%a%s}@." (indent (-4)) pp_stmt_list stmts
            (indent (-4))
    | CondS(e,ifthen,ifelse) ->
            fprintf ppf "%sif (%a)@\n" (indent 0) pp_expr e;
            incr_indent();
            pp_stmt ppf ifthen;
            decr_indent();
            if not (is_nil_stmt ifelse) then
                begin
                    fprintf ppf "%selse@\n" (indent 0);
                    incr_indent();
                    pp_stmt ppf ifelse;
                    decr_indent()
                end
    | SwitchS(e,stmt) ->
            fprintf ppf "%sswitch (%a)@\n" (indent 0) pp_expr e;
            incr_indent();
            pp_stmt ppf stmt;
            decr_indent()
    | WhileS(e,body) ->
            fprintf ppf "%swhile (%a)@\n" (indent 0) pp_expr e;
            incr_indent();
            pp_stmt ppf body;
            decr_indent()
    | DoS(e,body) ->
            fprintf ppf "%sdo@\n" (indent 0);
            incr_indent();
            pp_stmt ppf body;
            decr_indent();
            fprintf ppf "%swhile (%a);@." (indent 0) pp_expr e
    | ForS({ stmt_desc = ExprS e},cond,inc,body) ->
            fprintf ppf "%sfor (%a; %a; %a)@\n"
            (indent 0) pp_expr e pp_expr cond pp_expr inc;
            incr_indent();
            pp_stmt ppf body;
            decr_indent()
    | GotoS lbl        -> fprintf ppf "%sgoto %a;@." (indent 0) pp_identifier lbl
    | ContinueS        -> fprintf ppf "%scontinue;@." (indent 0)
    | BreakS           -> fprintf ppf "%sbreak;@." (indent 0)
    | ReturnS e ->
            if e.exp_desc = NilE
            then fprintf ppf "%sreturn;@." (indent 0)
            else fprintf ppf "%sreturn %a;@." (indent 0) pp_expr e
    | _ -> failwith "not reachable"

and pp_stmt_list ppf stmts =
    List.iter (fun stmt -> pp_stmt ppf stmt) stmts

let pp_fundecl ppf stor ty id body =
    pp_decllhs ppf (stor, ty, id);
    pp_print_newline ppf ();
    incr_indent();
    pp_stmt ppf body;
    decr_indent()

let pp_ext ppf ext = match ext.top_desc with
    | FunDecl(stor,ty,id,body) -> pp_fundecl ppf stor ty id body
    | ExtDecl decl -> pp_decl ppf decl
    | Directive file -> fprintf ppf "#%s@." file 

let f ppf ast =
    List.iter (fun ext -> fprintf ppf "%a@\n@\n" pp_ext ext) ast
