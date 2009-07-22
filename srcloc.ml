(* ソースコード内のロケーション情報 *)

open Lexing

type t = { loc_start: position; loc_end: position; }

let none = { loc_start = dummy_pos; loc_end = dummy_pos; }

let curr lexbuf = {
    loc_start = lexbuf.lex_start_p;
    loc_end = lexbuf.lex_curr_p;
}

let symbol_rloc () = {
    loc_start = Parsing.symbol_start_pos ();
    loc_end = Parsing.symbol_end_pos ();
}

let rhs_loc n = {
    loc_start = Parsing.rhs_start_pos n;
    loc_end = Parsing.rhs_end_pos n;
}
