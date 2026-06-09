(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* -------------------------------------------------------------------------- *)
(* --- Server API for Tactics                                             --- *)
(* -------------------------------------------------------------------------- *)

module P = Server.Package
module D = Server.Data
module R = Server.Request
module S = Server.States
module Md = Markdown
module AST = Server.Kernel_ast
module Jtactic = WpTipApi.Tactic

let package = P.package ~plugin:"wp" ~name:"tac"
    ~title:"WP Tactics" ()

(* -------------------------------------------------------------------------- *)
(* --- Tactical Kind                                                      --- *)
(* -------------------------------------------------------------------------- *)

module Jkind : D.S with type t = string =
struct
  include D.Jstring
  let jtype = D.declare ~package
      ~name:"kind" ~descr:(Md.plain "Parameter kind")
      (Junion [
          Jtag "checkbox";
          Jtag "spinner";
          Jtag "selector";
          Jtag "browser";
          Jtag "editor";
        ])
end

(* -------------------------------------------------------------------------- *)
(* --- Tactical Status                                                    --- *)
(* -------------------------------------------------------------------------- *)

module Jstatus : R.Output with type t = Tactical.status =
struct
  type t = Tactical.status
  let jtype = D.declare ~package
      ~name:"status" ~descr:(Md.plain "Tactical status")
      (Junion [
          Jtag "NotApplicable";
          Jtag "NotConfigured";
          Jtag "Applicable";
        ])
  let to_json (s : Tactical.status) =
    match s with
    | Not_applicable -> `String "NotApplicable"
    | Not_configured -> `String "NotConfigured"
    | Applicable _ -> `String "Applicable"
end

(* -------------------------------------------------------------------------- *)
(* --- Named Value Encoding                                               --- *)
(* -------------------------------------------------------------------------- *)

type value = V: 'a Tactical.named -> value

module Jvalue =
struct
  type t = value
  let jtype = D.declare ~package
      ~name:"value" ~descr:(Md.plain "Parameter option value")
      (Jrecord ["id",Jkey "value"; "label",Jstring; "title",Jstring])
  let of_json _ = D.failure "not implemented"
  let to_json (V a) = `Assoc [
      "id", `String a.vid ;
      "label", `String a.title ;
      "title", `String a.descr ;
    ]
end

let jvalues vlist = List.map (fun v -> V v) vlist

let joptions (type a)
    (values : a Tactical.named list)
    (equal : a -> a -> bool)
  : a D.data =
  let module M =
  struct
    type t = a
    let jtype = P.Jstring
    let to_json a =
      try `String (List.find (fun v -> equal v.Tactical.value a) values).vid
      with Not_found -> `Null
    let of_json js : t =
      let id = Json.string js in
      try (List.find (fun v -> v.Tactical.vid = id) values).value
      with Not_found -> D.failure "Incorrect value"
  end in (module M : (D.S with type t = a))

let jbrowser (type a) (find : a Tactical.finder)
  : a Tactical.named option D.data =
  let module M =
  struct
    type t = a Tactical.named option
    let jtype = P.Jstring
    let to_json = function
      | None -> `String ""
      | Some Tactical.{ vid } -> `String vid
    let of_json js =
      let id = Json.string js in
      try Some (find id) with Not_found -> None
  end in (module M : (D.S with type t = a Tactical.named option))

(* -------------------------------------------------------------------------- *)
(* --- Selection Data                                                     --- *)
(* -------------------------------------------------------------------------- *)

module Jselection : D.S with type t = Tactical.selection =
struct
  type t = Tactical.selection
  let jtype = D.Jtext.jtype
  let rec pp fmt = function
    | Tactical.Empty -> Format.fprintf fmt ""
    | Clause (Goal _) ->
      Format.fprintf fmt "@{<#goal>Goal@}"
    | Clause (Step s) ->
      Format.fprintf fmt "@{<#s%d>Hyp. #%d@}" s.id s.id
    | Inside (Goal _,t) ->
      let id = Lang.F.QED.id t in
      Format.fprintf fmt "@{<#goal>@{<#e%d>\"#%d\"@}@}" id id
    | Inside (Step s,t) ->
      let id = Lang.F.QED.id t in
      Format.fprintf fmt "@{<#s%d>@{<#e%d>\"#%d\"@}@}" s.id id id
    | Compose cc -> pc fmt cc
    | Multi _ -> ()
  and pc fmt = function
    | Tactical.Cint k -> Z.pretty fmt k
    | Range(a,b) -> Format.fprintf fmt "%d..%d" a b
    | Code(_,f,[]) -> Format.fprintf fmt "%s()" f
    | Code(_,f,x::xs) ->
      Format.fprintf fmt "@[<hov 2>%s(%a" f pp x ;
      List.iter (Format.fprintf fmt ",@ %a" pp) xs ;
      Format.fprintf fmt ")@]"
  let to_json s = D.jpretty pp s
  let of_json _ = Tactical.Empty
end

(* -------------------------------------------------------------------------- *)
(* --- Tactic Parameters & Fields                                         --- *)
(* -------------------------------------------------------------------------- *)

module Jparam = (val D.jkey ~kind:"param")

module Jparameter =
struct
  open D.Record
  type record
  let record : record signature = signature ()

  let id = field record ~name:"id"
      ~descr:(Md.plain "Parameter identifier") (module Jparam)

  let kind = field record ~name:"kind"
      ~descr:(Md.plain "Parameter kind") (module Jkind)

  let label = field record ~name:"label"
      ~descr:(Md.plain "Short name") (module D.Jstring)

  let title = field record ~name:"title"
      ~descr:(Md.plain "Description") (module D.Jstring)

  let enabled = field record ~name:"enabled"
      ~descr:(Md.plain "Enabled parameter")
      ~default:true (module D.Jbool)

  let value = field record ~name:"value"
      ~descr:(Md.plain "Value (with respect to kind)")
      (module D.Jany)

  let vmin = option record ~name:"vmin"
      ~descr:(Md.plain "Minimum range value (spinner only)")
      (module D.Jint)

  let vmax = option record ~name:"vmax"
      ~descr:(Md.plain "Maximum range value (spinner only)")
      (module D.Jint)

  let vstep = option record ~name:"vstep"
      ~descr:(Md.plain "Range step (spinner only)")
      (module D.Jint)

  let vlist = option record ~name:"vlist"
      ~descr:(Md.plain "List of options (selector only)")
      (module D.Jlist(Jvalue))

  include (val publish ~package
              ~name:"parameter"
              ~descr:(Md.plain "Parameter configuration")
              record)
end

class parameter
    ~(tactic : Tactical.t)
    ~(field : 'a Tactical.field)
    ~(kind : string)
    ~(data : 'a D.data)
    ?(range : int Tactical.range option)
    ?(options : 'a Tactical.named list option)
    () =
  let fd = Tactical.signature field in
  object(self)
    val mutable p_label = fd.title
    val mutable p_title = fd.descr
    val mutable p_vmin = None
    val mutable p_vmax = None
    val mutable p_vstep = None
    val mutable p_enabled = true
    initializer self#reset

    method id = Tactical.ident field

    method reset =
      begin
        p_label <- fd.title ;
        p_title <- fd.descr ;
        p_vmin <- Option.bind (fun rg -> rg.Tactical.vmin) range ;
        p_vmax <- Option.bind (fun rg -> rg.Tactical.vmax) range ;
        p_vstep <- Option.map (fun rg -> rg.Tactical.vstep) range ;
        p_enabled <- true ;
      end

    method import (js : D.json) =
      tactic#set_field field (D.data_of_json data js)

    method update ~id ?enabled ?title ?tooltip ?vmin ?vmax () =
      if id = fd.vid then
        begin
          Option.iter (fun e -> p_enabled <- e) enabled ;
          Option.iter (fun s -> p_label <- s) title ;
          Option.iter (fun s -> p_title <- s) tooltip ;
          if vmin <> None then p_vmin <- vmin ;
          if vmax <> None then p_vmax <- vmax ;
        end

    method export : D.json =
      let module J = Jparameter in
      J.default
      |> J.set J.id fd.vid
      |> J.set J.kind kind
      |> J.set J.label p_label
      |> J.set J.title p_title
      |> J.set J.value (tactic#get_field field |> D.data_to_json data)
      |> J.set J.enabled p_enabled
      |> J.set J.vmin p_vmin
      |> J.set J.vmax p_vmax
      |> J.set J.vstep p_vstep
      |> J.set J.vlist (Option.map jvalues options)
      |> J.to_json

  end

let make tactic (param : Tactical.parameter) : parameter =
  match param with
  | Checkbox field ->
    new parameter ~tactic ~field ~kind:"checkbox" ~data:D.jbool ()
  | Spinner(field,range) ->
    new parameter ~tactic ~field ~kind:"spinner" ~data:D.jint ~range ()
  | Selector(field,options,equal) ->
    let data = joptions options equal in
    new parameter ~tactic ~field ~kind:"selector" ~data ~options ()
  | Search(field,_,find) ->
    let data = jbrowser find in
    new parameter ~tactic ~field ~kind:"browser" ~data ()
  | Composer(field,_) ->
    new parameter ~tactic ~field ~kind:"editor" ~data:(module Jselection) ()

module ParameterConfig : D.S with type t = parameter =
struct
  type t = parameter
  let jtype = Jparameter.jtype
  let of_json _ = D.failure "not implemented"
  let to_json (p : parameter) = p#export
end

(* -------------------------------------------------------------------------- *)
(* --- Tactical Configuration                                             --- *)
(* -------------------------------------------------------------------------- *)

class configurator (tactic : Tactical.tactical) =
  let parameters = List.map (make tactic) tactic#params in
  object(self)
    val mutable local : Lang.F.pool option = None
    val mutable title = tactic#title
    val mutable descr = tactic#descr
    val mutable error = None
    val mutable status = Tactical.Not_applicable
    val mutable anchor : ProofEngine.node option = None
    val mutable target = Tactical.Empty

    (* Basics *)

    method reset =
      begin
        local <- None ;
        title <- tactic#title ;
        descr <- tactic#descr ;
        error <- None ;
        anchor <- None ;
        target <- Tactical.Empty ;
        List.iter (fun p -> p#reset) parameters ;
      end

    method id = tactic#id
    method params = parameters
    method lookup ~pid = List.find (fun p -> p#id = pid) parameters

    (* Feedback Interface *)

    method pool = Option.get local
    method interactive = true
    method has_error = error <> None
    method get_title = title
    method get_descr = descr
    method get_error = error

    method set_title : 'a. 'a Tactical.formatter =
      fun msg -> Format.kasprintf (fun m -> title <- m) msg

    method set_descr : 'a. 'a Tactical.formatter =
      fun msg -> Format.kasprintf (fun m -> descr <- m) msg

    method set_error : 'a. 'a Tactical.formatter =
      fun msg -> Format.kasprintf (fun m -> error <- Some m) msg

    method update_field :
      'a. ?enabled:bool -> ?title:string -> ?tooltip:string ->
      ?range:bool -> ?vmin:int -> ?vmax:int ->
      ?filter:(Lang.F.term -> bool) -> 'a Tactical.field -> unit =
      fun ?enabled ?title ?tooltip ?range ?vmin ?vmax ?filter field ->
      ignore range ;
      ignore filter ;
      let id = Tactical.ident field in
      List.iter
        (fun (p : parameter) ->
           p#update ~id ?enabled ?title ?tooltip ?vmin ?vmax ()
        ) parameters

    (* Processing *)

    method status = status

    method private select node pool selection =
      try
        local <- Some pool ;
        error <- None ;
        title <- tactic#title ;
        descr <- tactic#descr ;
        anchor <- Some node ;
        target <- selection ;
        status <- tactic#select (self :> Tactical.feedback) selection ;
        local <- None ;
      with exn ->
        local <- None ;
        status <- Not_applicable ;
        anchor <- None ;
        target <- Tactical.Empty ;
        error <- Some (Printf.sprintf "Error (%s)" (Printexc.to_string exn));
        if not @@ Cmdline.catch_at_toplevel exn then raise exn

    method configure node selection =
      let tree = ProofEngine.tree node in
      let pool = ProofEngine.pool tree in
      let ctxt = ProofEngine.tree_context tree in
      Lang.local ~pool
        (WpContext.on_context ctxt (self#select node pool))
        selection

    method private commit tree node process =
      try
        let jtactic = ProofScript.jtactic tactic target in
        let fork = ProofEngine.fork tree ~anchor:node jtactic process in
        let children = snd @@ ProofEngine.commit fork in
        ProofEngine.forward tree ;
        List.map snd children
      with exn ->
        local <- None ;
        anchor <- None ;
        target <- Tactical.Empty ;
        error <- Some (Printf.sprintf "Error (%s)" (Printexc.to_string exn));
        if not @@ Cmdline.catch_at_toplevel exn then raise exn ; []

    method apply =
      match anchor, status with
      | Some node, Applicable process ->
        let tree = ProofEngine.tree node in
        let ctxt = ProofEngine.tree_context tree in
        WpContext.on_context ctxt (self#commit tree node) process
      | _ -> []

  end

(* -------------------------------------------------------------------------- *)
(* --- Tactical Status                                                    --- *)
(* -------------------------------------------------------------------------- *)

(* indexed by tactical Id. *)
let index : (string,configurator) Hashtbl.t = Hashtbl.create 0

let configurator tactic =
  let id = tactic#id in
  try Hashtbl.find index id
  with Not_found ->
    let cfg = new configurator tactic in
    Hashtbl.add index id cfg ; cfg

let iter f = Tactical.iter (fun t -> f @@ configurator t)

let tactics =
  let model : configurator S.model = S.model () in
  S.column model ~name:"label" ~descr:(Md.plain "Tactic name")
    ~data:(module D.Jstring) ~get:(fun cfg -> cfg#get_title) ;
  S.column model ~name:"title" ~descr:(Md.plain "Tactic description")
    ~data:(module D.Jstring) ~get:(fun cfg -> cfg#get_descr) ;
  S.option model ~name:"error" ~descr:(Md.plain "Tactic error")
    ~data:(module D.Jstring) ~get:(fun cfg -> cfg#get_error) ;
  S.column model ~name:"status" ~descr:(Md.plain "Tactic status")
    ~data:(module Jstatus) ~get:(fun cfg -> cfg#status) ;
  S.column model ~name:"params" ~descr:(Md.plain "Configuration parameters")
    ~data:(module D.Jlist(ParameterConfig))
    ~get:(fun cfg -> cfg#params) ;
  S.register_array
    ~package
    ~name:"tactical"
    ~descr:(Md.plain "Tactical Configurations")
    ~key:(fun cfg -> cfg#id)
    ~keyName:"id"
    ~keyType:Jtactic.jtype
    ~iter model

(* -------------------------------------------------------------------------- *)
(* --- Tactical Target Configuration                                      --- *)
(* -------------------------------------------------------------------------- *)

let configure jtactic node =
  let sequent = snd @@ Wpo.compute @@ ProofEngine.goal node in
  match ProofScript.configure jtactic sequent with
  | None -> ()
  | Some(tactical,selection) ->
    begin
      let console = (configurator tactical :> Tactical.feedback) in
      console#set_title "%s" tactical#title ;
      WpTipApi.setSelection node selection ;
    end

let () =
  R.register ~package
    ~kind:`EXEC
    ~name:"configureTactics"
    ~descr:(Md.plain "Configure all tactics")
    ~input:(module WpTipApi.Node)
    ~output:(module D.Junit)
    ~signals:[WpTipApi.printStatus]
    begin fun node ->
      match ProofEngine.tactical node with
      | None ->
        let selection = WpTipApi.selection node in
        iter (fun cfg -> cfg#configure node selection) ;
        S.reload tactics ;
      | Some jtactic ->
        let ctxt = ProofEngine.node_context node in
        WpContext.on_context ctxt (configure jtactic) node ;
        S.reload tactics ;
    end

(* -------------------------------------------------------------------------- *)
(* --- Tactical Parameter Configuration                                   --- *)
(* -------------------------------------------------------------------------- *)

let () =
  let setParameter = R.signature ~output:(module D.Junit) () in
  let get_node = R.param setParameter ~name:"node"
      ~descr:(Md.plain "Proof node target") (module WpTipApi.Node) in
  let get_tactic = R.param setParameter ~name:"tactic"
      ~descr:(Md.plain "Tactic to configure") (module Jtactic) in
  let get_param = R.param setParameter ~name:"param"
      ~descr:(Md.plain "Parameter to configure") (module Jparam) in
  let get_value = R.param setParameter ~name:"value"
      ~descr:(Md.plain "New parameter value") (module D.Jany) in
  R.register_sig ~package ~kind:`EXEC
    ~name:"setParameter"
    ~descr:(Md.plain "Configure tactical parameter")
    setParameter
    begin fun rq () ->
      let node = get_node rq in
      let tac = get_tactic rq in
      let pid = get_param rq in
      let cfg = configurator tac in
      let prm = cfg#lookup ~pid in
      let selection = WpTipApi.selection node in
      prm#import (get_value rq) ;
      cfg#configure node selection ;
      S.update tactics cfg ;
    end

(* -------------------------------------------------------------------------- *)
(* --- Applying Tactic                                                    --- *)
(* -------------------------------------------------------------------------- *)

let () = R.register
    ~package ~kind:`EXEC
    ~name:"applyTactic"
    ~descr:(Md.plain "Applies the (configured) tactic")
    ~input:(module Jtactic)
    ~output:(module D.Jlist(WpTipApi.Node))
    begin fun tactic ->
      let cfg = configurator tactic in
      let children = cfg#apply in
      S.update tactics cfg ;
      children
    end

let () = R.register
    ~package ~kind:`EXEC
    ~name:"applyTacticAndProve"
    ~descr:(Md.plain "Applies tactic and run provers on children")
    ~input:(module Jtactic)
    ~output:(module D.Jlist(WpTipApi.Node))
    begin fun tactic ->
      let cfg = configurator tactic in
      let children = cfg#apply in
      S.update tactics cfg ;
      List.iter WpTipApi.runProvers children ;
      children
    end


(* -------------------------------------------------------------------------- *)
