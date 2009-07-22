let preprocessor = "cpp"
let preprocessor_args = ["-Wall"; "-pedantic"]

let preprocess src =
    let cmd = Printf.sprintf "%s %s %s"
        preprocessor (String.concat " " preprocessor_args) src in
    Unix.open_process_in cmd

let output = ref "a.out"
let compile sources =
    Printf.printf "compiling .. \n%!";
    let cmd = Printf.sprintf "gcc %s -o %s" (String.concat " " sources) !output in
    let exit_code = match Unix.system cmd with
        | Unix.WEXITED code -> code
        | _ -> failwith "compile"
    in exit_code

let parse inchan =
    Idtable.enter_new_namespace();  (* グローバル名前空間 *)
    Parser.translation_unit Lexer.token (Lexing.from_channel inchan)

let translate src =
    let out = (Str.global_replace (Str.regexp "\\.sc$") ".c" src) in
    Printf.printf "translate %s -> %s ..\n%!" src out;
    let input_chan = preprocess src in
    try
        let ast = parse input_chan in
        Parsing.clear_parser();
        let output_chan = open_out out in
        try
            Pptree.f (Format.formatter_of_out_channel output_chan) ast;
            out
        with e -> (close_out output_chan; raise e)
    with e -> (close_in input_chan; raise e)

(* エントリーポイント *)
let () = 
    try
        let files = ref[] in
        Arg.parse 
        [("-o", Arg.String (fun o -> output := o), "output name")]
        (fun src -> files := !files @ [src])
        ("SINCL: SINCL Is New C-Language\n" ^
        Printf.sprintf "usage: %s file..." Sys.argv.(0));
        let c_sources = List.map (fun src -> translate src) !files in
        exit (compile c_sources)
    with e ->
        Error.report_error Format.err_formatter e;
        exit 2
