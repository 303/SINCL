open Format

let report_error ppf exn =
    let report ppf = function
        | Lexer.Error(err, loc) ->
                (* Srcloc.print ppf loc; 後回し *)
                Lexer.report_error ppf err
        | Parsererr.Error(err, loc) ->
                (* Srcloc.print ppf loc; 後回し *)
                Parsererr.report_error ppf err
        | e -> fprintf ppf "@]"; raise e
    in fprintf ppf "@[%a@]@." report exn
