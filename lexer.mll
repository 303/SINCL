(* レキサ *)
{
open Tree
open Lexing
open Parser
open Format

type error =
  | Unterminated_string
exception Error of error * Srcloc.t

let report_error ppf = function
    | Unterminated_string ->
        fprintf ppf "String literal not terminated"

let keyword_table = Util.create_hashtable_from_list 100 [
    "break"    , Tbreak;
    "case"     , Tcase;
    "char"     , Tchar;
    "const"    , Tconst;
    "continue" , Tcontinue;
    "default"  , Tdefault;
    "do"       , Tdo;
    "double"   , Tdouble;
    "else"     , Telse;
    "enum"     , Tenum;
    "extern"   , Textern;
    "float"    , Tfloat;
    "for"      , Tfor;
    "goto"     , Tgoto;
    "if"       , Tif;
    "int"      , Tint;
    "long"     , Tlong;
    "register" , Tregister;
    "restrict" , Trestrict;
    "return"   , Treturn;
    "short"    , Tshort;
    "signed"   , Tsigned;
    "sizeof"   , Tsizeof;
    "static"   , Tstatic;
    "struct"   , Tstruct;
    "switch"   , Tswitch;
    "typedef"  , Ttypedef;
    "union"    , Tunion;
    "unsigned" , Tunsigned;
    "void"     , Tvoid;
    "volatile" , Tvolatile;
    "while"    , Twhile
    ]

let update_loc lexbuf file line absolute chars =
  let pos = lexbuf.lex_curr_p in
  let new_file = match file with
                 | None -> pos.pos_fname
                 | Some s -> s
  in
  lexbuf.lex_curr_p <- { pos with
    pos_fname = new_file;
    pos_lnum = if absolute then line else pos.pos_lnum + line;
    pos_bol = pos.pos_cnum - chars;
  }


let escaped_char = function
    | '\'' -> '\''
    | '\\' -> '\\'
    | 'b'  -> '\b'
    | 'n'  -> '\n'
    | 'r'  -> '\r'
    | 't'  -> '\t'
    | 'a'  -> '\007' (* OCamlにない文字なのでコードで *)
    | 'v'  -> '\011'
    | 'f'  -> '\012'
    | '0'  -> '\000' (* ナル文字もない *)
    | c    -> c

(* 以下はOCamlから持ってきたコード *)
let string_start_loc = ref Srcloc.none;;
let initial_string_buffer = String.create 256
let string_buff = ref initial_string_buffer
let string_index = ref 0

let reset_string_buffer () =
  string_buff := initial_string_buffer;
  string_index := 0

let store_string_char c =
  if !string_index >= String.length (!string_buff) then begin
    let new_buff = String.create (String.length (!string_buff) * 2) in
      String.blit (!string_buff) 0 new_buff 0 (String.length (!string_buff));
      string_buff := new_buff
  end;
  String.unsafe_set (!string_buff) (!string_index) c;
  incr string_index

let get_stored_string () =
  let s = String.sub (!string_buff) 0 (!string_index) in
  string_buff := initial_string_buffer;
  s

}

let newline  = ('\n' | '\r' | "\r\n")
let blank    = [' ' '\t']
let letter   = ['_' 'a'-'z' 'A'-'Z']
let space    = [' ' '\t' '\n' '\r']
let nonzero  = ['1'-'9']
let digit    = ['0'-'9']
let octdigit = ['0'-'7']
let hexdigit = ['0'-'9' 'a'-'f' 'A'-'F']

let identifier = letter (letter | digit)*


let decinteger = nonzero digit*
let octinteger = '0' octdigit*
let hexinteger = ("0x" | "0X") hexdigit+
let integer    = decinteger | octinteger | hexinteger

let exponent   = ['e' 'E'] ['+' '-']? digit+
let floating   = (digit* '.' digit+|digit+ '.') exponent?

rule token = parse
    | newline { update_loc lexbuf None 1 false 0; token lexbuf }
    | space+  { token lexbuf }    (* 読み飛ばす *)
    | "//" [^ '\n' '\r']* newline
    { update_loc lexbuf None 1 false 0; token lexbuf }
    | "/*"  { comment lexbuf }
    | "*="  { Tstarasgn }
    | "/="  { Tdivasgn }
    | "%="  { Tmodasgn }
    | "+="  { Taddasgn }
    | "-="  { Tsubasgn }
    | "<<=" { Tlshiftasgn }
    | ">>=" { Trshiftasgn }
    | "&="  { Tampasgn }
    | "|="  { Torasgn }
    | "^="  { Txorasgn }
    | "->"  { Tarrow }
    | "++"  { Tplusplus }
    | "--"  { Tminusminus }
    | "<<"  { Tlshift }
    | ">>"  { Trshift }
    | "<="  { Tle }
    | ">="  { Tge }
    | "=="  { Teq }
    | "!="  { Tne }
    | "&&"  { Tseqand }
    | "||"  { Tseqor }
    | "..." { Tellipsis }
    | ','   { Tcomma }
    | '='   { Tasgn }
    | '?'   { Tquestion }
    | ':'   { Tcolon }
    | '|'   { Tor }
    | '^'   { Txor }
    | '&'   { Tamp }
    | '~'   { Ttilde }
    | '!'   { Tbang }
    | '<'   { Tlt }
    | '>'   { Tgt }
    | '+'   { Tplus }
    | '-'   { Tminus }
    | '*'   { Tstar }
    | '/'   { Tdiv }
    | '%'   { Tmod }
    | '['   { Tlbracket }
    | ']'   { Trbracket }
    | '{'   { Tlbrace }
    | '}'   { Trbrace }
    | '('   { Tlparen }
    | ')'   { Trparen }
    | ';'   { Tsemi }
    | '.'   { Tdot }
    | '\'' [^ '\\' '\''] '\''
    {
        (* charは整数にしてしまう *)
        let c = Lexing.lexeme_char lexbuf 1 in
        Tinteger_cst (make_char_cst c)
    }
    | "'\\" ['\\' '\'' 'a' 'b' 'f' 'n' 'r' 't' 'v'] '\''
    {
        (* charは整数にしてしまう *)
        let c = escaped_char (Lexing.lexeme_char lexbuf 2) in
        Tinteger_cst (make_char_cst c)
    }
    | integer ['l' 'L']?
    {
        (* intも32ビットということにする *)
        let value = Int64.of_string (Lexing.lexeme lexbuf) in
        Tinteger_cst (make_int32_cst false value)
    }
    | integer ((['u' 'U'] ['l' 'L']?) | (['l' 'L']? ['u' 'U']))
    {
        (* intも32ビットということにする *)
        let value = Int64.of_string (Lexing.lexeme lexbuf) in
        Tinteger_cst (make_int32_cst true value)
    }
    | integer ("ll" | "LL")
    {
        (* long long *)
        let value = Int64.of_string (Lexing.lexeme lexbuf) in
        Tinteger_cst (make_int64_cst false value)
    }
    | integer ((['u' 'U'] ("ll" | "LL")?) | (("ll" | "LL")? ['u' 'U']))
    {
        (* unsigned long long *)
        (* Int64.to_stringはオーバーフローし得るのでScanfを使う *)
        let value = Util.read_uint64 (Lexing.lexeme lexbuf) in
        Tinteger_cst (make_int64_cst true value)
    }
    | floating
    {
        (* double *)
        let cst = { real_kind = Real_Double; repr = Lexing.lexeme lexbuf; } in
        Tfloating_cst (Const_Real cst)
    }
    | floating ['f' 'F']
    {
        (* float *)
        let cst = { real_kind = Real_Float; repr = Lexing.lexeme lexbuf; } in
        Tfloating_cst (Const_Real cst)
    }
    | floating ['l' 'L']
    {
        (* long double *)
        let cst = { real_kind = Real_LongDouble; repr = Lexing.lexeme lexbuf; } in
        Tfloating_cst (Const_Real cst)
    }
    | '"'
    {
        reset_string_buffer();
        let string_start = lexbuf.lex_start_p in
        string_start_loc := Srcloc.curr lexbuf;
        string lexbuf;
        lexbuf.lex_start_p <- string_start;
        Tstring_literal (Const_String (get_stored_string()))
    }
    | identifier
    {
        let name = Lexing.lexeme lexbuf in
        try
            Hashtbl.find keyword_table name (* 予約語 *)
        with Not_found ->
        try
            let id = Idtable.find_normal_ident name in
            match id.id_kind with
            | Ident_TypedefName -> Ttypedef_name id
            | Ident_EnumConst   -> Tenum_const id
            | _ -> Tidentifier id
        with Not_found ->
            let id = make_identifier Ident_Unknown void_type name !Idtable.namespace_id
            in Tidentifier id
    }
    | '#' ([^ '\n' '\r' ]* as cmd) newline
    { update_loc lexbuf None 1 false 0; Tdirective cmd }
    | eof   { Teof }
and string = parse
    | '"'
    { () }
    | '\\' newline ([' ' '\t'] * as space)
    { 
        update_loc lexbuf None 1 false (String.length space);
        string lexbuf
    }
    | '\\' ['\\' '\'' '"' 'n' 't' 'b' 'r' ' ']
    {
        store_string_char(escaped_char(Lexing.lexeme_char lexbuf 1));
        string lexbuf }
    | newline
    {
        update_loc lexbuf None 1 false 0;
        let s = Lexing.lexeme lexbuf in
        for i = 0 to String.length s - 1 do
            store_string_char s.[i];
        done;
        string lexbuf
    }
    | eof
    { raise (Error (Unterminated_string, !string_start_loc)) }
    | _
    { store_string_char(Lexing.lexeme_char lexbuf 0); string lexbuf }
and comment = parse
    | "*/"      { token lexbuf }
    | newline   { update_loc lexbuf None 1 false 0; comment lexbuf }
    | _         { comment lexbuf }
