let create_hashtable_from_list num ls = 
    let tbl = Hashtbl.create num in
    List.iter (fun (k, v) -> Hashtbl.add tbl k v) ls; tbl

(* OCamlにはunsigned整数のサポートがないっぽい。
 * しかし，Printf/Scanfにはフォーマットで%uが使えるのでこれでとりあえず代用
 *)
let read_uint64 str =
    let v = ref Int64.zero in
    Scanf.bscanf (Scanf.Scanning.from_string str) "%Lu" (fun x -> v := x); !v
