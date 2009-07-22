open Format
open Tree

type error =
    | Undefined_identifier of string
    | Identifier_redefined of string
    | Identifier_mismatch of Tree.ident

exception Error of error * Srcloc.t

let report_error ppf = function
    | Undefined_identifier name ->
            fprintf ppf "Identifier %s undefined" name
    | Identifier_redefined name ->
            fprintf ppf "Identifier %s redefined" name
    | Identifier_mismatch id ->
            fprintf ppf "Identifier %s is declared as %s" id.id_name
            (Tree.ident_kind_to_string id.id_kind)

