open Parsererr
open Tree

type decl_spec =
    | Decl_PointerTo of int
    | Decl_ArrayOf of expression option * int
    | Decl_Function of declaration list

type declarator = {
    decl_id   : Tree.ident;
    decl_spec : decl_spec list;
    decl_init : expression option;
}

let add_declspec decl spec = {
    decl with decl_spec = decl.decl_spec @ spec
}

let construct_type base dspecs = 
    let ty = ref base in
    List.iter (fun d ->
        match d with
        | Decl_PointerTo qual -> ty := {
            ity_desc = PointerT !ty;
            ity_qual = qual;
        }
        | Decl_ArrayOf(len, qual) -> ty := {
            ity_desc = ArrayT(!ty, len);
            ity_qual = qual;
        }
        | Decl_Function params -> ty := {
            ity_desc = FunctionT(!ty, params);
            ity_qual = 0;
        }
    ) (List.rev dspecs);
    !ty

let deconstruct_type ty =
    let rec iter t dspecs =
        match t.ity_desc with
            | PointerT ety ->
                    iter ety (Decl_PointerTo t.ity_qual :: dspecs)
            | ArrayT(ety,len)   ->
                    iter ety (Decl_ArrayOf(len, t.ity_qual) :: dspecs)
            | FunctionT(ret,params) ->
                    iter ret (Decl_Function params :: dspecs)
            | _ -> (t, dspecs)
    in iter ty []

let construct_abstract_sue_type loc id =
    let ty = {
        ity_desc = CompositeT (id, ref None);
        ity_qual = 0;
    } in
    id.id_type <- ty

let construct_sue_type id decls =
    match id.id_type.ity_desc with
        | CompositeT(_,d)      -> d := Some decls
        | _ -> failwith "construct_sue_type"

