(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Server

let package =
  Package.package ~plugin:"studia" ~name:"studia" ~title:"Studia" ()

type effects = {
  direct: Printer_tag.localizable list;
  indirect: Printer_tag.localizable list
}

let empty = { direct = []; indirect = []; }

module Effects = struct
  open Server.Data

  type record
  let record: record Record.signature = Record.signature ()

  let direct = Record.field record ~name:"direct"
      ~descr:(Markdown.plain "List of statements with direct effect.")
      (module Data.Jlist (Kernel_ast.Marker))
  let indirect = Record.field record ~name:"indirect"
      ~descr:(Markdown.plain "List of statements with indirect effect.")
      (module Data.Jlist (Kernel_ast.Marker))

  let data = Record.publish record ~package ~name:"effects"
      ~descr:(Markdown.plain "Statements that read or write a location.")

  module R : Record.S with type r = record = (val data)
  type t = effects
  let jtype = R.jtype

  let to_json effects =
    R.default |>
    R.set direct effects.direct |>
    R.set indirect effects.indirect |>
    R.to_json
end

let stmt_marker = Printer_tag.localizable_of_stmt
let global_marker vi = Printer_tag.localizable_of_declaration (SGlobal vi)

let compute_writes zone =
  try
    let reads = Writes.compute zone in
    let add acc = function
      | Writes.Assign stmt | CallDirect stmt ->
        { acc with direct = stmt_marker stmt :: acc.direct }
      | CallIndirect stmt ->
        { acc with indirect = stmt_marker stmt :: acc.indirect }
      | FormalInit (_vi, callsites) ->
        let calls = List.concat_map snd callsites in
        { acc with direct = List.map stmt_marker calls @ acc.direct }
      | GlobalInit (vi, _initinfo) ->
        { acc with direct = global_marker vi :: acc.direct }
    in
    List.fold_left add empty reads
  with exn ->
    Options.warning "Error when computing writes (%s)"
      (Printexc.to_string exn) ;
    empty

let compute_reads zone =
  try
    let reads = Reads.compute zone in
    let add acc = function
      | Reads.Direct stmt ->
        { acc with direct = stmt_marker stmt :: acc.direct }
      | Indirect stmt ->
        { acc with indirect = stmt_marker stmt :: acc.indirect }
    in
    List.fold_left add empty reads
  with exn ->
    Options.warning "Error when computing reads (%s)"
      (Printexc.to_string exn) ;
    empty

let lval_location kinstr lval =
  Eva.Results.(before_kinstr kinstr |> eval_address lval |> as_zone)

let tlval_location kinstr tlval =
  let cvalue = Eva.Results.(before_kinstr kinstr |> get_cvalue_model) in
  let term = Logic_const.term (TLval tlval) (Cil.typeOfTermLval tlval) in
  let access = Locations.Read in
  let zones = Eva.Logic_inout.tlval_to_zones Code_annot cvalue access term in
  match zones with
  | Some zones -> zones.over
  | None ->
    Data.failure "Cannot evaluate the memory location of %a"
      Printer.pp_term_lval tlval

let marker_location (marker: Printer_tag.localizable) =
  match marker with
  | PLval (_kf, kinstr, lval) -> lval_location kinstr lval
  | PGlobal (GVar (vi, _, _) | GVarDecl (vi, _))
  | PVDecl (_, _, vi) -> Locations.zone_of_varinfo vi
  | PTermLval (_kf, kinstr, _property, tlval) -> tlval_location kinstr tlval
  | _ -> Data.failure "%a is not an lvalue" Printer_tag.pp_localizable marker

let () = Request.register ~package
    ~kind:`GET ~name:"getReadsLval"
    ~descr:(Markdown.plain "Get the list of statements that read a lval.")
    ~input:(module Kernel_ast.Marker)
    ~output:(module Effects)
    (fun marker -> compute_reads (marker_location marker))

let () = Request.register ~package
    ~kind:`GET ~name:"getWritesLval"
    ~descr:(Markdown.plain "Get the list of statements that write a lval.")
    ~input:(module Kernel_ast.Marker)
    ~output:(module Effects)
    (fun marker -> compute_writes (marker_location marker))
