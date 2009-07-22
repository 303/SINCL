(* 識別子表 *)

open Parsererr
open Tree

let f() = Printf.printf "HOGEHOGE\n"

let idtable_normal = Hashtbl.create 389
let idtable_sue    = Hashtbl.create 53 
let idtable_label  = Hashtbl.create 53 
let brace_stack    = Stack.create() (* 名前区間IDスタック *)

let namespace_id      = ref 0          (* 名前区間ID *)
let last_namespace_id = ref 0          (* 最も最後に出た名前空間ID *)

exception Found

let enter_new_namespace() =
    Stack.push !namespace_id brace_stack;
    incr namespace_id

let leave_namespace() =
    last_namespace_id := Stack.pop brace_stack

(* 一番最後に出た名前空間にもう一度入る． 関数定義の部分で必要 *)
let reenter_namespace() =
    Stack.push !last_namespace_id brace_stack

let clear_label_table() = Hashtbl.clear idtable_label

let find_normal_ident name =
    let id = ref dummy_ident in 
    try
        Stack.iter (fun sid ->
            try
                id := Hashtbl.find idtable_normal (name, sid);
                raise Found
            with Not_found -> ()) brace_stack;
        raise Not_found
    with Found -> !id

let register_normal_ident loc kind ty name =
    let sid = Stack.top brace_stack in
    if Hashtbl.mem idtable_normal (name, sid)
    then raise (Error(Identifier_redefined name, loc))
    else
    let id = make_identifier kind ty name sid in
    Hashtbl.add idtable_normal (name, sid) id;
    id

let check_normal_ident loc id kind =
    if id.id_kind <> Ident_Unknown && id.id_kind <> kind then
        raise (Error(Identifier_mismatch id, loc))
    else id.id_kind <- kind

let get_label_ient name = 
    try
        Hashtbl.find idtable_label name
    with Not_found ->
    let id = make_identifier Ident_Label void_type name 0 in
    Hashtbl.add idtable_label name id;
    id

let get_sue_ident loc kind name =
    try
        let id = Hashtbl.find idtable_sue name in
        if id.id_kind <> kind then
            raise (Error(Identifier_mismatch id, loc))
        else id
    with Not_found ->
    let id = make_identifier kind void_type name 0 in
    Type.construct_abstract_sue_type loc id;
    Hashtbl.add idtable_sue name id;
    id
