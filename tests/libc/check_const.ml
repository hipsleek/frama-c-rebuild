open Cil_types

(* known exceptions to the const/valid_read rule *)
let non_const_exceptions = [
  "putenv";
]

let warn_if_const string typ vi loc =
  let bs = if string = "" then "\\" else "" in
  if Ast_types.has_qualifier "const" typ then
    Kernel.result ~source:(fst loc)
      "'requires %svalid%s' of a const variable %a. \
       You probably meant '%svalid_read%s' instead"
      bs string Printer.pp_varinfo vi bs string

let warn_if_not_const kf string typ vi loc =
  if not (List.mem (Kernel_function.get_name kf) non_const_exceptions) then
    if not (Ast_types.has_qualifier "const" typ) then
      Kernel.result ~source:(fst loc)
        "'requires \\valid_read%s' of a non-const variable %a. \
         You may have meant '\\valid%s'"
        string Printer.pp_varinfo vi string

let check_annot kf _ (a: identified_predicate) =
  let p = (Logic_const.pred_of_id_pred a).pred_content in
  match p with
  | Pvalid (_, t) | Pvalid_read (_, t)
  | Papp ({l_var_info={lv_name=("valid_string"|"valid_read_string")}},
          _, [t]) ->
    begin
      let warn = match p with
        | Pvalid _ ->
          warn_if_const ""
        | Papp ({l_var_info={lv_name="valid_string"}},_,_) ->
          warn_if_const "_string"
        | Pvalid_read _ ->
          warn_if_not_const kf ""
        | Papp ({l_var_info={lv_name="valid_read_string"}},_,_) ->
          warn_if_not_const kf "_string"
        | _ -> assert false
      in
      match t.term_node with
      | TAddrOf (TVar lvi, _) -> begin
          match lvi.lv_origin with
          | Some ({vtype = typ} as vi) ->
            warn typ vi t.term_loc
          | _ -> ()
        end
      | TBinOp ((PlusPI | MinusPI),
                ({term_node = TLval (TVar lvi, _)} |
                 {term_node = TCast (false,_, {term_node = TLval (TVar lvi, _)})}),
                _)
      | TLval (TVar lvi, _) -> begin
          match lvi.lv_origin with
          | Some vi ->
            warn (Ast_types.direct_pointed_type vi.vtype) vi t.term_loc
          | _ -> ()
        end
      | _ -> ()
    end
  | _ -> ()

let check () =
  let check_kf kf =
    let bhvs = Annotations.behaviors kf in
    List.iter (fun bhv ->
        Annotations.iter_requires (check_annot kf) kf bhv.b_name) bhvs
  in
  Globals.Functions.iter check_kf

let () = Boot.Main.extend check
