open Cil_types

let rec ghost_status fmt lval =
  let t = Ast_types.unroll (Cil.typeOfLval lval) in
  let ghost = Ast_types.is_ghost t in

  Format.fprintf fmt "%s" (if ghost then "ghost" else "normal") ;
  match t.tnode with
  | TPtr(_) ->
    Format.fprintf fmt " -> %a" pointed_ghost_status lval
  | TArray(_) ->
    Format.fprintf fmt " -> %a" in_array_ghost_status lval
  | TComp(_) ->
    Format.fprintf fmt " -> %a" comp_ghost_status lval
  | _ -> ()
and pointed_ghost_status fmt lval =
  let loc = Current_loc.get() in
  let exp = Cil.new_exp (Lval lval) ~loc in
  let lval = Mem(exp), NoOffset in
  Format.fprintf fmt "%a" ghost_status lval
and in_array_ghost_status fmt lval =
  let loc = Current_loc.get() in
  let lval = Cil.addOffsetLval (Index((Cil.zero ~loc), NoOffset)) lval in
  Format.fprintf fmt "%a" ghost_status lval
and comp_ghost_status fmt lval =
  match (Cil.typeOfLval lval).tnode with
  | TComp { cfields } ->
    Format.fprintf fmt "{ " ;
    List.iter (field_ghost_status fmt lval) (Option.value ~default:[] cfields) ;
    Format.fprintf fmt " }"
  | _ -> assert false
and field_ghost_status fmt lval f =
  let lval = Cil.addOffsetLval (Field(f, NoOffset)) lval in
  Format.fprintf fmt "%s: %a" f.fname ghost_status lval

class visitor = object(_)
  inherit Visitor.frama_c_inplace

  method! vvdec v =
    Kernel.feedback "%a@. %a: %a"
      Fileloc.pretty (Current_loc.get())
      Cil_datatype.Varinfo.pretty v
      ghost_status (Cil.var v) ;
    Cil.DoChildren
end

let () =
  Boot.Main.extend (fun () -> Visitor.visitFramacFileSameGlobals (new visitor) (Ast.get ()))
