(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* Messages longer than N characters are truncated when printed on terminal. *)
let max_message_length = 10000

type kind = Result | Feedback | Debug | Warning | Error | Failure

[@@@ warning "-32"]

let pretty_kind fmt = function
  | Result -> Format.fprintf fmt "Result"
  | Feedback -> Format.fprintf fmt "Feedback"
  | Debug -> Format.fprintf fmt "Debug"
  | Warning -> Format.fprintf fmt "Warning"
  | Error -> Format.fprintf fmt "Error"
  | Failure -> Format.fprintf fmt "Failure"

[@@@ warning "+32"]

type event = {
  evt_kind : kind ;
  evt_plugin : string ;
  evt_category : string option;
  evt_source : Filepos.t option ;
  evt_message : Rich_text.t ;
}

(* -------------------------------------------------------------------------- *)
(* --- Exception Management                                               --- *)
(* -------------------------------------------------------------------------- *)

exception FeatureRequest of Filepos.t option * string * string
exception AbortError of string (* plug-in *)
exception AbortFatal of string (* plug-in *)

(* -------------------------------------------------------------------------- *)
(* --- Terminal Management                                                --- *)
(* -------------------------------------------------------------------------- *)

type lock =
  | Ready
  | Locked
  | DelayedLock

type terminal = {
  mutable lock : lock ;
  mutable isatty : bool ;
  mutable clean : bool ;
  mutable delayed : (terminal -> unit) list ;
  mutable formatter : Format.formatter ;
}

let delayed_echo t =
  match t.lock with
  | Locked -> true
  | Ready | DelayedLock -> false

let is_locked t =
  match t.lock with
  | Locked | DelayedLock -> true
  | Ready -> false

let is_ready t =
  match t.lock with
  | Locked | DelayedLock -> false
  | Ready -> true

let term_clean t =
  if t.isatty && not t.clean then
    begin
      (* TERM escape commands:
         "\r" is carriage return ;
         "\027[K" is CSI command EL 'Erase in Line' ;
         See https://en.wikipedia.org/wiki/ANSI_escape_code
      *)
      Format.pp_print_string t.formatter "\r\027[K" ;
      t.clean <- true ;
    end

let set_terminal t isatty formatter =
  begin
    (* Ensures previous terminal state is clean *)
    assert (is_ready t) ;
    term_clean t ;
    (* Now reconfigure the terminal *)
    t.isatty <- isatty ;
    t.formatter <- formatter;
    t.clean <- true ;
  end

let stdout = {
  lock = Ready ;
  clean = true ;
  delayed = [] ;
  isatty = Unix.isatty Unix.stdout ;
  formatter = Format.std_formatter ;
}

let clean () = term_clean stdout

let set_formatter ?(isatty=false) formatter =
  set_terminal stdout isatty formatter

let reset_stdout ~isatty () =
  set_terminal stdout isatty Format.std_formatter


(* -------------------------------------------------------------------------- *)
(* --- Locked Formatter                                                   --- *)
(* -------------------------------------------------------------------------- *)

let lock_terminal t =
  begin
    if is_locked t then
      failwith "Console is already locked" ;
    term_clean t ;
    t.lock <- Locked ;
    t.formatter
  end

let unlock_terminal t fmt =
  if is_ready t then
    failwith "Console can not be unlocked" ;
  begin
    Format.pp_print_flush fmt () ;
    t.lock <- Ready ;
    List.iter
      (fun job -> job t)
      (List.rev t.delayed) ;
    t.delayed <- [] ;
  end

let print_on_output job =
  let fmt = lock_terminal stdout in
  try job fmt ; unlock_terminal stdout fmt
  with error -> unlock_terminal stdout fmt ; raise error

(* -------------------------------------------------------------------------- *)
(* --- Delayed Lock until first write                                     --- *)
(* -------------------------------------------------------------------------- *)

let formatter_with ?output:new_output ?flush:new_flush fmt =
  let old_output, old_flush = Format.pp_get_formatter_output_functions fmt () in
  let output = match new_output with
    | None -> old_output
    | Some output -> output old_output
  and flush = match new_flush with
    | None -> old_flush
    | Some flush -> flush old_flush
  in
  let new_fmt = Format.make_formatter output flush in
  let stag_functions =  Format.pp_get_formatter_stag_functions fmt () in
  Format.pp_set_formatter_stag_functions new_fmt stag_functions;
  Format.pp_set_mark_tags new_fmt (Format.pp_get_mark_tags fmt ());
  Format.pp_set_print_tags new_fmt (Format.pp_get_print_tags fmt ());
  new_fmt

let delayed_terminal terminal =
  if is_locked terminal then
    failwith "Console is already locked" ;
  terminal.lock <- DelayedLock ;
  let delayed = ref true in
  let output regular_output text k n =
    if !delayed then begin
      terminal.lock <- Locked ;
      delayed := false ;
    end;
    regular_output text k n
  and flush regular_flush () =
    if not !delayed then (* otherwise, nothing to flush yet ! *)
      regular_flush ()
  in
  formatter_with ~output ~flush terminal.formatter

let print_delayed job =
  let fmt = delayed_terminal stdout in
  try job fmt ; unlock_terminal stdout fmt
  with error -> unlock_terminal stdout fmt ; raise error

(* -------------------------------------------------------------------------- *)
(* --- Echo Line(s)                                                       --- *)
(* -------------------------------------------------------------------------- *)

let formatter_with_indentation fmt amount =
  let blank = String.make amount ' ' in
  let beginning_of_line = ref false in
  let rec output old_output text p n =
    if n > 0 then begin
      (* Output indentation on each beginning of a line *)
      if !beginning_of_line then old_output blank 0 amount;
      match String.index_from_opt text p '\n' with
      | Some t when t >= 0 && t <= p + n ->
        (* complete line *)
        let len = t + 1 - p in
        old_output text p len;
        beginning_of_line := true;
        output old_output text (t + 1) (n - len)
      | _ ->
        (* incomplete or last line *)
        old_output text p n;
        beginning_of_line := false;
    end
  in
  formatter_with ~output fmt

(* -------------------------------------------------------------------------- *)
(* --- Events                                                             --- *)
(* -------------------------------------------------------------------------- *)

module Event =
struct
  type t = event

  let pp_source fmt = function
    | None -> ()
    | Some pos ->
      Format.fprintf fmt "%a: @?" Filepos.pretty pos

  let pp_category fmt = function
    | None -> ()
    | Some name ->
      Format.fprintf fmt ":%s" name

  let pp_kind fmt = function
    | Result | Feedback | Debug -> ()
    | Error   -> Format.fprintf fmt "@{<red>User Error:@} "
    | Warning -> Format.fprintf fmt "@{<orange>Warning:@} "
    | Failure -> Format.fprintf fmt "@{<red>Failure:@} "

  let pp_message ?truncate fmt buffer =
    if Rich_text.need_truncation ?truncate buffer then
      Format.pp_print_string fmt "(truncated message) ";
    Rich_text.pretty ?truncate fmt buffer

  let pretty ?truncate fmt evt =
    let header = Rich_text.mprintf ~trim:false "@{<bold>[%s%a] %a%a@}"
        evt.evt_plugin
        pp_category evt.evt_category
        pp_source evt.evt_source
        pp_kind evt.evt_kind
    in
    let pp_header fmt header = Rich_text.pretty fmt header in
    let long_header = match evt with
      | { evt_category = None ; evt_source = None } -> false
      | _ -> true
    in
    (* whenever the header-part shall be separated from the message-part *)
    let lonely_header =
      long_header &&
      (Rich_text.size header + Rich_text.size evt.evt_message > 80 ||
       Rich_text.contains evt.evt_message '\n')
    in
    let fmt = formatter_with_indentation fmt 2 in
    Format.fprintf fmt "%a%t%a@."
      pp_header header
      (fun fmt -> if lonely_header then Format.pp_force_newline fmt ())
      (pp_message ?truncate) evt.evt_message

  let message event =
    Rich_text.to_string event.evt_message
end

let echo_event evt terminal =
  term_clean terminal ;
  let truncate = if terminal.isatty then `Middle max_message_length else `None in
  Event.pretty ~truncate terminal.formatter evt

let do_echo terminal evt =
  if delayed_echo terminal then
    terminal.delayed <- echo_event evt :: terminal.delayed
  else
    echo_event evt terminal

let do_transient terminal ~plugin message =
  if not (Rich_text.is_empty message) && not (delayed_echo terminal) then
    begin
      term_clean terminal ;
      let fmt = terminal.formatter in
      Format.fprintf fmt "@{<bold>[%s]@} " plugin ;
      let width = max 40 (77 - String.length plugin) in
      let ellipsis = "…" in
      let limit =
        try String.length ellipsis + Rich_text.index message '\n'
        with Not_found -> width in
      Rich_text.pretty ~truncate:(`Right limit) ~ellipsis fmt message ;
      if terminal.isatty
      then terminal.clean <- false
      else Format.pp_print_newline fmt () ;
    end

(* -------------------------------------------------------------------------- *)
(* --- Source                                                             --- *)
(* -------------------------------------------------------------------------- *)

let source ~file ~line =
  Filepos.make ~path:file ~line ~column:0 ~offset:0 ()

let current_loc = ref (fun () -> raise Not_found)

let set_current_source fpos = current_loc := fpos

let get_current_source () = !current_loc ()

let get_source current = function
  | None -> if current then Some (!current_loc ()) else None
  | Some _ as s -> s

(* -------------------------------------------------------------------------- *)
(* --- Channels                                                           --- *)
(* -------------------------------------------------------------------------- *)

type emitter = {
  mutable listeners : (event -> unit) list ;
  mutable echo : bool ;
}

type ontty = [
  | `Message   (* Normal message (default) *)
  | `Feedback  (* Temporary visible on console, normal message otherwise *)
  | `Transient (* Temporary visible, only on console *)
  | `Silent    (* Not visible on console *)
]

type channel = {
  locked_buffer : Rich_text.buffer ; (* already allocated top-level buffer *)
  mutable stack : int ;   (* number of 'stacked' buffers *)
  plugin : string ;
  emitters : emitter array ;
  terminal : terminal ;
}

type channelstate =
  | NotCreatedYet of emitter array
  | Created of channel

let nth_kind = function
  | Result   -> 0
  | Feedback -> 1
  | Debug    -> 2
  | Error    -> 3
  | Warning  -> 4
  | Failure  -> 5

let all_kinds = [| Result ; Feedback ; Debug ; Error ; Warning ; Failure |]

let () = Array.iteri
    (fun i k -> assert (i == nth_kind k))
    all_kinds

(* -------------------------------------------------------------------------- *)
(* --- Channels                                                           --- *)
(* -------------------------------------------------------------------------- *)

let all_channels : (string,channelstate) Hashtbl.t = Hashtbl.create 31
let default_emitters =
  Array.map (fun _ -> { listeners=[] ; echo=true })
    all_kinds

let new_emitters () =
  Array.map (fun e -> { listeners = e.listeners ; echo = e.echo })
    default_emitters

let get_emitters plugin =
  try
    match Hashtbl.find all_channels plugin with
    | NotCreatedYet e -> e
    | Created c -> c.emitters
  with Not_found ->
    let e = new_emitters () in
    Hashtbl.replace all_channels plugin (NotCreatedYet e) ; e


let new_channel plugin =
  let create_with_emitters plugin emitters =
    let c = {
      plugin = plugin ;
      stack = 0 ;
      locked_buffer = Rich_text.Buffer.create () ;
      emitters = emitters ;
      terminal = stdout ;
    } in
    Hashtbl.replace all_channels plugin (Created c) ; c
  in
  try
    match Hashtbl.find all_channels plugin with
    | Created c -> c
    | NotCreatedYet ems -> create_with_emitters plugin ems
  with Not_found ->
    let ems = new_emitters () in
    create_with_emitters plugin ems

(* -------------------------------------------------------------------------- *)
(* --- Already emitted messages                                           --- *)
(* -------------------------------------------------------------------------- *)

let check_not_yet = ref (fun _evt -> false)

(* -------------------------------------------------------------------------- *)
(* --- Listeners                                                          --- *)
(* -------------------------------------------------------------------------- *)

let do_fire e f = f e

let iter_kind ?kind f ems =
  match kind with
  | None -> Array.iter f ems
  | Some ks -> List.iter (fun k -> f ems.(nth_kind k)) ks

let iter_plugin ?plugin ?kind f =
  match plugin with
  | None ->
    Hashtbl.iter
      (fun _ s ->
         match s with
         | Created c -> iter_kind ?kind f c.emitters
         | NotCreatedYet ems -> iter_kind ?kind f ems)
      all_channels ;
    iter_kind ?kind f default_emitters
  | Some p ->
    iter_kind ?kind f (get_emitters p)

let add_listener ?plugin ?kind demon =
  iter_plugin ?plugin ?kind (fun em -> em.listeners <- em.listeners @ [demon])

let set_echo ?plugin ?kind echo =
  iter_plugin ?plugin ?kind (fun em -> em.echo <- echo)

let notify e =
  let es = get_emitters e.evt_plugin in
  List.iter (fun f -> f e) es.(nth_kind e.evt_kind).listeners

(* -------------------------------------------------------------------------- *)
(* --- Generic Log Routine                                                --- *)
(* -------------------------------------------------------------------------- *)

let open_buffer c =
  if c.stack > 0 then
    ( c.stack <- succ c.stack ; Rich_text.Buffer.create () )
  else
    ( c.stack <- 1 ; c.locked_buffer )

let close_buffer c =
  if c.stack > 1 then
    c.stack <- pred c.stack
  else
    Rich_text.Buffer.reset c.locked_buffer

let logtransient channel format =
  let buffer = open_buffer channel in
  Rich_text.Buffer.kbprintf
    (fun _fmt ->
       try
         let message = Rich_text.Buffer.contents ~trim:true buffer in
         do_transient channel.terminal ~plugin:channel.plugin message ;
         close_buffer channel
       with e ->
         close_buffer channel ;
         raise e
    ) buffer format

let locked_listeners = ref false

let logwithfinal finally channel
    ?(fire=true)     (* fire channel listeners *)
    ?(kind=Feedback) (* message kind *)
    ?category        (* message category *)
    ?(current=false) (* use current source as default *)
    ?source          (* source location *)
    ?emitwith        (* additional emitter *)
    ?(echo=true)     (* echo on terminal *)
    ?(once=false)    (* log and emit only once *)
    ?(append=ignore) (* additional text *)
    text =
  let source = get_source current source in
  let buffer = open_buffer channel in
  Rich_text.Buffer.bprintf buffer "%a" Format.pp_open_vbox 0 ;
  Rich_text.Buffer.kbprintf
    (fun fmt ->
       try
         append fmt;
         Format.pp_close_box fmt () ;
         Format.pp_print_newline fmt () ;
         Format.pp_print_flush fmt () ;
         let message = Rich_text.Buffer.contents buffer in
         let output =
           if not (Rich_text.is_empty message) then
             let event = {
               evt_kind = kind ;
               evt_plugin = channel.plugin ;
               evt_category = category ;
               evt_message = message ;
               evt_source = source ;
             } in
             if not once || !check_not_yet event then
               begin
                 let e = channel.emitters.(nth_kind kind) in
                 if echo && e.echo then
                   do_echo channel.terminal event ;
                 Option.iter (do_fire event) emitwith;
                 if fire && not !locked_listeners then
                   begin
                     try
                       locked_listeners := true ;
                       List.iter (do_fire event) e.listeners ;
                       locked_listeners := false ;
                     with exn ->
                       locked_listeners := false ;
                       raise exn
                   end ;
                 Some event
               end
             else None
           else None
         in
         close_buffer channel ;
         finally output
       with e ->
         close_buffer channel ;
         raise e
    ) buffer text

let finally_unit _ = ()
let finally_raise e _ = raise e
let finally_false _ = false

type deferred_exn =
  | DNo_exn
  | DWarn_as_error of event
  | DError of event
  | DFatal of event

let deferred_exn = ref DNo_exn

let unreported_error = "##unreported-error##"

(* we keep track of at most one deferred exception, ordered by seriousness
   (internal error > user error > warning-as-error). the rationale is that
   an internal error might cause subsequent errors or warning, but the reverse
   is not true: an deferred user error must not lead to an internal error.
   Should that ever happen, at the very least the code should be modified to
   directly [abort] instead of merely logging an [error].
*)
let update_deferred_exn exn =
  match !deferred_exn, exn with
  | DNo_exn, _ -> deferred_exn := exn
  | DWarn_as_error _, DWarn_as_error _ -> ()
  | DWarn_as_error _, _ -> deferred_exn := exn
  | DError _, (DNo_exn | DWarn_as_error _ | DError _) -> ()
  | DError _, DFatal _ -> deferred_exn := exn
  | DFatal _, _ -> ()

let warn_event_as_error event = update_deferred_exn (DWarn_as_error event)

let deferred_raise ~fatal event msg =
  (* reset deferred flag. *)
  let () = deferred_exn := DNo_exn in
  let channel = new_channel event.evt_plugin in
  let pp_pos fmt pos = Format.fprintf fmt "%a: " Filepos.pretty pos in
  let pp_pos_opt = Pretty_utils.pp_opt pp_pos in
  let print_event fmt =
    Format.fprintf fmt "@\n%a%a"
      pp_pos_opt event.evt_source
      (Event.pp_message ?truncate:None) event.evt_message
  in
  let append = Some print_event in
  let exn =
    if fatal then AbortFatal event.evt_plugin
    else AbortError event.evt_plugin
  in
  let finally = finally_raise exn in
  (* change the kind to avoid re-appending 'Error' to the message *)
  logwithfinal finally channel ?append ~kind:Result msg

let treat_deferred_error () =
  match !deferred_exn with
  | DNo_exn -> ()
  | DWarn_as_error event ->
    let wkey =
      match event.evt_category with
      | None -> ""
      | Some s when s = unreported_error -> ""
      | Some s -> s
    in
    deferred_raise ~fatal:false { event with evt_kind = Error }
      "Deferred error: warning as error %s:" wkey
  | DError event ->
    deferred_raise ~fatal:false event
      "Deferred error message was emitted during execution:"
  | DFatal event ->
    deferred_raise ~fatal:true event
      "Deferred internal error message was emitted during execution:"

(* -------------------------------------------------------------------------- *)
(* --- Messages Interface                                                 --- *)
(* -------------------------------------------------------------------------- *)

type 'a pretty_printer =
  ?current:bool -> ?source:Filepos.t ->
  ?emitwith:(event -> unit) -> ?echo:bool -> ?once:bool ->
  ?append:(Format.formatter -> unit) ->
  ('a,Format.formatter,unit) format -> 'a

type ('a,'b) pretty_aborter =
  ?current:bool -> ?source:Filepos.t -> ?echo:bool ->
  ?append:(Format.formatter -> unit) ->
  ('a,Format.formatter,unit,'b) format4 -> 'a

let log_channel channel ?(kind=Result) ?current ?source ?emitwith ?echo ?once
    ?append text =
  logwithfinal finally_unit channel ~kind ?current ?source ?emitwith ?echo ?once
    ?append text

let echo e =
  try
    match Hashtbl.find all_channels e.evt_plugin with
    | NotCreatedYet _ -> raise Not_found
    | Created c -> do_echo c.terminal e
  with Not_found ->
    let msg =
      Format.asprintf "[unknown channel %s]:%a"
        e.evt_plugin
        (Event.pp_message ?truncate:None) e.evt_message
    in failwith msg

(* ------------------------------------------------------------------------- *)
(* --- Plug-in Interface                                                 --- *)
(* ------------------------------------------------------------------------- *)

module Category_trie =
struct
  (* No Datatype at this level for dependencies reasons *)
  module String_map = Map.Make(String)

  type 'a t =
    | Node of 'a option * 'a t String_map.t

  let empty = Node (None, String_map.empty)

  let rec add_structure l t =
    match l with
    | [] -> t
    | x :: l ->
      let Node (info, map) = t in
      let binding =
        try String_map.find x map
        with Not_found -> Node (info, String_map.empty)
      in
      let res = add_structure l binding in
      Node (info, String_map.add x res map)

  let rec add_info l ?merge info (Node (old_info, map)) =
    match l with
    | [] ->
      let rec aux map =
        String_map.map
          (function Node(old_info, map) ->
             let new_info =
               match old_info, merge with
               | None, _ | _, None -> Some info
               | Some old_info, Some merge -> Some (merge old_info info)
             in
             Node (new_info, aux map)) map
      in
      Node (Some info, aux map)
    | x :: l ->
      let binding = String_map.find x map in
      let res = add_info l info binding in
      Node (old_info, String_map.add x res map)

  let rec get l (Node(info, map)) =
    match l with
    | [] -> info
    | x :: l ->
      let binding = String_map.find x map in
      get l binding

  let fold f map acc =
    let rec aux suf (Node(info, map)) acc =
      let acc = f (List.rev suf) info acc in
      String_map.fold (fun s t acc -> aux (s::suf) t acc) map acc
    in aux [] map acc

  let suffixes l trie =
    let rec aux res suf l (Node(_,map)) =
      match l with
      | [] ->
        let res = (List.rev suf) :: res in
        String_map.fold (fun s t res -> aux res (s::suf) [] t) map res
      | x::l ->
        let t = String_map.find x map in
        aux res (x::suf) l t
    in
    (* Provide results in lexicographic order. *)
    List.rev (aux [] [] l trie)
end

let rec split_joker = function
  | [] -> []
  | ["*"] -> []
  | ""::w -> split_joker w
  | a::w -> a::split_joker w

let split_category s = split_joker (String.split_on_char ':' s)

let evt_category = function
  | { evt_category = None } -> []
  | { evt_category = Some s } -> split_category s

(* a is a sub-category of b *)
let rec is_subcategory a b = match a,b with
  | _,[] -> true
  | [],_ -> false
  | a1::aw , b1::bw -> a1 = b1 && is_subcategory aw bw

let merge_category l =
  match l with
  | [] -> "*"
  | [ s ] -> s
  | hd :: tl ->
    let b = Buffer.create 15 in
    Buffer.add_string b hd;
    List.iter (fun s -> Buffer.add_char b ':'; Buffer.add_string b s) tl;
    Buffer.contents b

type warn_status =
  | Winactive
  | Wfeedback_once
  | Wfeedback
  | Wonce
  | Wactive
  | Werror_once
  | Werror
  | Wabort

let pp_warn_status fmt s =
  let s =
    match s with
    | Winactive -> "inactive"
    | Wfeedback_once -> "feedback-once"
    | Wfeedback -> "feedback"
    | Wonce -> "warning-once"
    | Wactive -> "warning"
    | Werror_once -> "error-once"
    | Werror -> "error"
    | Wabort -> "abort"
  in
  Format.pp_print_string fmt s

let merge_status old_status new_status =
  match old_status, new_status with
  | Winactive, Wactive -> Wactive
  | Winactive, Wonce -> Wonce
  | Winactive, _ -> Winactive
  | _ -> new_status

type category_action = Category_help | Change_category of (bool * string) list
let parse_category s =
  let categories = String.split_on_char ',' s in
  if List.mem "help" categories then Category_help
  else
    let parse_single s =
      match String.get s 0 with
      | '-' -> false, String.sub s 1 (String.length s - 1)
      | '+' -> true, String.sub s 1 (String.length s - 1)
      | _ -> true, s
    in
    let non_empty_category s =
      if s <> "" then Some (parse_single s) else None
    in
    Change_category (List.filter_map non_empty_category categories)

type warning_action =
  | Warning_help
  | Set_status of (string * warn_status) list
  | Parsing_error of string

let warn_status_of_string = function
  | "inactive" | "ignore" -> Winactive
  | "feedback-once" -> Wfeedback_once
  | "feedback" -> Wfeedback
  | "warning-once" | "warn-once" | "once" -> Wonce
  | "warning" | "warn" | "active" -> Wactive
  | "error-once" | "err-once" -> Werror_once
  | "error" | "err" -> Werror
  | "abort" -> Wabort
  | s -> invalid_arg (Format.sprintf "Unknown warning category status `%s'" s)

let parse_warning s =
  let directives = String.split_on_char ',' s in
  if List.mem "help" directives then Warning_help
  else
    let parse_single s =
      match String.split_on_char '=' s with
      | [] -> assert false (* split_on_char should return at least an element
                              even if it is the empty string *)
      | [ c ] -> (c, Wactive)
      | [ c; status ] -> (c, warn_status_of_string status)
      | _ -> invalid_arg (Format.sprintf "Ill-formed warn key directive `%s'" s)
    in
    try
      let non_empty_warning s =
        if s <> "" then Some (parse_single s) else None
      in
      Set_status (List.filter_map non_empty_warning directives)
    with Invalid_argument msg -> Parsing_error msg

module type Level = sig
  val value_if_set: int option ref
  val get: unit -> int
  val set: int -> unit
end

module Make_level(X: sig val default: int end) = struct
  let value_if_set = ref None
  let get () = match !value_if_set with None -> X.default | Some x -> x
  let set n = value_if_set := Some n
end

module type Messages =
sig

  type category

  type warn_category

  val verbose_atleast: int -> bool
  val debug_atleast: int -> bool

  val printf : ?level:int -> ?dkey:category ->
    ?current:bool -> ?source:Filepos.t ->
    ?append:(Format.formatter -> unit) ->
    ?header:(Format.formatter -> unit) ->
    ('a,Format.formatter,unit) format -> 'a

  val result  : ?level:int -> ?dkey:category -> 'a pretty_printer
  val has_tty : unit -> bool
  val feedback: ?ontty:ontty -> ?level:int -> ?dkey:category -> 'a pretty_printer
  val debug   : ?level:int -> ?dkey:category -> 'a pretty_printer
  val warning : ?wkey: warn_category -> 'a pretty_printer
  val error   : 'a pretty_printer
  val abort   : ('a,'b) pretty_aborter
  val failure : 'a pretty_printer
  val fatal   : ('a,'b) pretty_aborter
  val verify  : bool -> ('a,bool) pretty_aborter

  val not_yet_implemented : ?current:bool -> ?source:Filepos.t ->
    ('a,Format.formatter,unit,'b) format4 -> 'a
  val deprecated : string -> now:string -> ('a -> 'b) -> 'a -> 'b

  val with_result  : (event option -> 'b) -> ('a,'b) pretty_aborter
  val with_warning : (event option -> 'b) -> ('a,'b) pretty_aborter
  val with_error   : (event option -> 'b) -> ('a,'b) pretty_aborter
  val with_failure : (event option -> 'b) -> ('a,'b) pretty_aborter

  val log : ?kind:kind -> ?verbose:int -> ?debug:int -> 'a pretty_printer

  val logwith : (event option -> 'b) -> ?wkey:warn_category ->
    ?emitwith:(event -> unit) -> ?once:bool -> ('a,'b) pretty_aborter

  val register : kind -> (event -> unit) -> unit (** Very local listener. *)

  val register_tag_handlers : (string -> string) * (string -> string) -> unit

  val register_category: ?help:string -> ?default:bool -> string -> category

  val pp_category: Format.formatter -> category -> unit

  val pp_all_categories: unit -> unit

  val dkey_name: category -> string
  val get_category_help: category -> string

  val is_registered_category: string -> bool

  val get_category: string -> category option
  val get_all_categories: unit -> category list

  val add_debug_keys: category -> unit
  val del_debug_keys: category -> unit
  val get_debug_keys: unit -> category list

  val is_debug_key_enabled: category -> bool

  val register_warn_category:
    ?help:string -> ?default:warn_status -> string -> warn_category

  val is_warn_category: string -> bool

  val pp_warn_category: Format.formatter -> warn_category -> unit

  val pp_all_warn_categories_status: unit -> unit

  val wkey_name: warn_category -> string

  val get_warn_category: string -> warn_category option

  val get_all_warn_categories: unit -> warn_category list

  val get_all_warn_categories_status: unit -> (warn_category * warn_status) list

  val set_warn_status: warn_category -> warn_status -> unit

  val get_warn_status: warn_category -> warn_status

end

module Register
    (P : sig
       val channel : string
       val label : string
       val verbose_atleast : int -> bool
       val debug_atleast : int -> bool
     end) =
struct

  include P

  type category = string

  type warn_category = string

  let categories = ref Category_trie.empty

  let categories_help : ((string, string) Hashtbl.t) = Hashtbl.create 5

  let () = Hashtbl.add categories_help "*" "All categories"

  let not_registered s =
    failwith (s ^ " is not a registered category for " ^ label)

  let change_debug_key_status s b =
    try categories := Category_trie.add_info (split_category s) b !categories
    with Not_found -> not_registered s

  let add_debug_keys s = change_debug_key_status s true
  let del_debug_keys s = change_debug_key_status s false

  let register_category ?(help="No description provided") ?(default=false)
      (s:string) =
    let l = split_category s in
    categories := Category_trie.add_structure l !categories;
    Hashtbl.replace categories_help s help;
    if default then add_debug_keys s;
    s

  let pp_category fmt (cat: category) = Format.pp_print_string fmt cat

  let get_category_help (cat: category) =
    match Hashtbl.find_opt categories_help cat with
    | None -> "Not registered directly (see subcategory descriptions)"
    | Some help -> help

  let get_all_categories () =
    List.map merge_category (Category_trie.suffixes [] !categories)

  let is_registered_category s =
    List.mem (split_category s) (Category_trie.suffixes [] !categories)

  let get_category s =
    if is_registered_category s then Some s else None

  let dkey_name s = s

  let wkey_name s = s

  let get_debug_keys () =
    let f cat info acc =
      match info with
      | None | Some false -> acc
      | Some true -> (merge_category cat) :: acc
    in
    Category_trie.fold f !categories []

  let is_debug_key_enabled (c:category) =
    let s = (c:>string) in
    match Category_trie.get (split_category s) !categories with
    | None -> false
    | Some flag -> flag
    | exception Not_found -> not_registered s

  let has_debug_key = function
    | None -> true (* No key means to be displayed each time *)
    | Some c -> is_debug_key_enabled c

  let warn_categories = ref Category_trie.empty

  let warn_categories_help : ((string, string) Hashtbl.t) = Hashtbl.create 5

  let () = Hashtbl.add warn_categories_help "*" "All warning categories"

  let wnot_registered s =
    failwith (s ^ " is not a registered warning category for " ^ label)

  let set_warn_status s status =
    try
      warn_categories :=
        Category_trie.add_info
          (split_category s) ~merge:merge_status status !warn_categories
    with Not_found -> wnot_registered s

  let register_warn_category ?(help="No description provided") ?default s =
    let l = split_category s in
    warn_categories := Category_trie.add_structure l !warn_categories;
    Hashtbl.replace warn_categories_help s help;
    Option.iter (set_warn_status s) default;
    s

  let get_warn_category_help (cat: category) =
    match Hashtbl.find_opt warn_categories_help cat with
    | None -> "Not registered directly (see subcategory descriptions)"
    | Some help -> help

  let get_all_warn_categories () =
    List.map merge_category (Category_trie.suffixes [] !warn_categories)

  let get_all_warn_categories_status () =
    List.rev
      (Category_trie.fold
         (fun cat status l  ->
            (merge_category cat, Option.value ~default:Wactive status) :: l)
         !warn_categories [])

  let is_warn_category s =
    List.mem (split_category s) (Category_trie.suffixes [] !warn_categories)

  let pp_warn_category fmt s = Format.pp_print_string fmt s

  let get_warn_category s = if is_warn_category s then Some s else None

  let get_warn_status s =
    match Category_trie.get (split_category s) !warn_categories with
    | Some s -> s
    | None -> Wactive
    | exception Not_found -> wnot_registered s

  let std = new_channel P.channel

  let internal_register_tag_handlers _c (_ope,_close) = ()
  (* BM->LOIC: I need to keep this code around to be able to handle
     marks and tags correctly.
     Do you think we can emulate all other features of Log but without
     using c.buffer at all?
     Everything but ensure_unique_newline seems feasible.
     See Design.make_slash to see a useful example.

     let start_of_line= Printf.sprintf "\n[%s] " P.label in
     let length= pred (String.length start_of_line) in
     Format.pp_set_all_formatter_output_functions c.formatter
     ~out:c.term.output
     ~flush:c.term.flush
     ~newline:(fun () -> c.term.output start_of_line 0 length)
     ~spaces:(fun _ ->  ()(*TODO:correct margin*))
     ;
     Format.pp_set_tags c.formatter true;
     Format.pp_set_mark_tags c.formatter true;
     Format.pp_set_print_tags c.formatter false;
     Format.pp_set_formatter_tag_functions c.formatter
     {(Format.pp_get_formatter_tag_functions c.formatter ())
     with
     Format.mark_open_tag = ope;
     mark_close_tag = close}
  *)

  let register_tag_handlers h =
    internal_register_tag_handlers std h

  let to_be_log verbose debug =
    match verbose , debug with
    | 0 , 0 -> verbose_atleast 1
    | v , 0 -> verbose_atleast v
    | 0 , d -> debug_atleast d
    | v , d -> verbose_atleast v || debug_atleast d


  let log ?(kind=Result) ?(verbose=0) ?(debug=0) ?current ?source ?emitwith
      ?echo ?once ?append text =
    if to_be_log verbose debug then
      logwithfinal finally_unit std ~kind ?current ?source ?emitwith ?echo
        ?once ?append text
    else Pretty_utils.nullprintf text

  let result ?(level=1) ?dkey ?current ?source ?emitwith ?echo ?once ?append
      text =
    if verbose_atleast level && has_debug_key dkey then
      logwithfinal finally_unit std ~kind:Result ?category:dkey ?current
        ?source ?emitwith ?echo ?once ?append text
    else Pretty_utils.nullprintf text

  let transient channel = channel.terminal.isatty

  let has_tty () = transient std

  let feedback ?(ontty=`Message) ?(level=1) ?dkey ?current ?source ?emitwith
      ?echo ?once ?append text =
    let mode =
      if verbose_atleast level && has_debug_key dkey
      then
        match ontty with
        | `Feedback -> if transient std then `Transient else `Message
        | `Transient -> if transient std then `Transient else `Silent
        | `Silent -> if transient std then `Silent else `Message
        | `Message -> `Message
      else `Silent
    in match mode with
    | `Message ->
      logwithfinal finally_unit std ~kind:Feedback ?category:dkey ?current
        ?source ?emitwith ?echo ?once ?append text
    | `Transient -> logtransient std text
    | `Silent -> Pretty_utils.nullprintf text

  let should_output_debug level dkey =
    match level, dkey with
    | None, None -> debug_atleast 1
    | Some l, None -> debug_atleast l
    | None, Some _ -> has_debug_key dkey
    | Some l, Some _ -> debug_atleast l && has_debug_key dkey

  let debug ?level ?dkey ?current ?source ?emitwith ?echo ?once ?append text =
    if should_output_debug level dkey then
      logwithfinal finally_unit std ~kind:Debug ?category:dkey ?current
        ?source ?emitwith ?echo ?once ?append text
    else
      Pretty_utils.nullprintf text

  let force_error = function
    | None ->
      { evt_kind = Failure;
        evt_plugin = std.plugin;
        evt_category = Some unreported_error;
        evt_message = Rich_text.of_string "Silent error";
        evt_source = None
      }
    | Some evt -> evt

  let finally_user_error evt =
    let evt = force_error evt in update_deferred_exn (DError evt)

  let finally_internal_error evt =
    let evt = force_error evt in update_deferred_exn (DFatal evt)

  let error ?current ?source ?emitwith ?echo ?once ?append text =
    logwithfinal finally_user_error std ~kind:Error ?current ?source
      ?emitwith ?echo ?once ?append text

  let abort ?current ?source ?echo ?append text =
    logwithfinal (finally_raise (AbortError P.channel)) std ~kind:Error
      ?current ?source ?echo ?append text

  let failure ?current ?source ?emitwith ?echo ?once ?append text =
    logwithfinal finally_internal_error std ~kind:Failure ?current ?source
      ?emitwith ?echo ?once ?append text

  let fatal ?current ?source ?echo ?append text =
    logwithfinal (finally_raise (AbortFatal P.channel)) std ~kind:Failure
      ?current ?source ?echo ?append text

  let verify assertion ?current ?source ?echo ?append text =
    if assertion then
      Format.kfprintf (fun _ -> true) Pretty_utils.null text
    else
      logwithfinal finally_false std ~kind:Failure ?current ?source ?echo
        ?append text

  let logwith finally ?(wkey="") ?emitwith ?once ?current ?source ?echo ?append
      text =
    let status = get_warn_status wkey in
    let kind =
      match status with
      | Wfeedback | Wfeedback_once -> Feedback
      | Wactive | Wonce | Winactive -> Warning
      | Werror | Werror_once -> Error
      | Wabort -> Failure
    in
    if status <> Winactive && (kind <> Feedback || verbose_atleast 1)  then
      begin
        let action, once_suffix =
          match status with
          | Wabort ->
            Some (fun _ -> abort "warning %s treated as fatal error." wkey), ""
          | Werror -> Some warn_event_as_error, ""
          | Werror_once ->
            Some
              (fun evt ->
                 warn_event_as_error evt; set_warn_status wkey Winactive),
            "warn-error-once"
          | Wfeedback_once ->
            Some (fun _ -> set_warn_status wkey Winactive), "warn-feedback-once"
          | Wonce ->
            Some (fun _ -> set_warn_status wkey Winactive), "warn-once"
          | Wactive | Winactive | Wfeedback -> None, ""
        in
        let emitwith =
          match emitwith, action with
          | None, None -> None
          | Some e, None | None, Some e -> Some e
          | Some e1, Some e2 -> Some (fun evt -> e1 evt; e2 evt)
        in
        let category = if wkey = "" then None else Some wkey in
        let append_once_suffix = (fun fmt ->
            Format.fprintf fmt
              "@.(%s: no further messages from category '%s' will be emitted)"
              once_suffix wkey)
        in
        let append = if once_suffix = "" then append
          else match append with
            | None -> Some append_once_suffix
            | Some app ->
              Some (fun fmt -> app fmt; append_once_suffix fmt)
        in
        logwithfinal finally std ~kind ?category ?current ?source ?emitwith
          ?once ?echo ?append text
      end
    else Pretty_utils.with_null (fun () -> finally None) text

  let warning ?wkey ?current ?source ?emitwith ?echo ?once ?append text =
    logwith finally_unit ?wkey ?current ?source ?emitwith ?echo ?once ?append
      text

  let with_result finally ?current ?source ?echo ?append text =
    logwithfinal finally std ~kind:Result ?current ?source ?echo ?append
      text

  let with_warning finally ?current ?source ?echo ?append text =
    logwithfinal finally std ~kind:Warning ?current ?source ?echo ?append
      text

  let with_error finally ?current ?source ?echo ?append text =
    logwithfinal finally std ~kind:Error ?current ?source ?echo ?append
      text

  let with_failure finally ?current ?source ?echo ?append text =
    logwithfinal finally std ~kind:Failure ?current ?source ?echo ?append
      text

  let register kd f =
    let em = std.emitters.(nth_kind kd) in
    em.listeners <- em.listeners @ [f]

  let not_yet_implemented ?(current=false) ?source text =
    let buffer = Buffer.create 80 in
    let source = get_source current source in
    let finally fmt =
      Format.pp_print_flush fmt ();
      let msg = Buffer.contents buffer in
      raise (FeatureRequest(source,std.plugin,msg)) in
    let fmt = Format.formatter_of_buffer buffer in
    Format.kfprintf finally fmt text

  let deprecated name ~now f x =
    warning ~once:true
      "call to deprecated function '%s'.\nShould use '%s' instead."
      name now ;
    f x

  let noprint _fmt = ()

  let spynewline bol output buffer start length =
    begin
      let ofs = start+length-1 in
      if 0 <= ofs && ofs < String.length buffer then
        bol := buffer.[ofs] = '\n' ;
      output buffer start length
    end

  let printf ?(level=1) ?dkey ?current ?source ?(append=noprint) ?header text =
    if verbose_atleast level && has_debug_key dkey then
      begin
        (* Header is a regular message *)
        let header = match header with None -> noprint | Some h -> h in
        logwithfinal finally_unit std ~fire:false ~kind:Result
          ?category:dkey ?current ?source  "%t" header;
        let bol = ref true in
        let fmt = delayed_terminal stdout in
        let fmt = formatter_with ~output:(spynewline bol) fmt in
        try
          Format.kfprintf
            begin fun fmt ->
              append fmt ;
              unlock_terminal stdout fmt ;
              if not !bol then Format.pp_print_newline fmt () ;
            end
            fmt text
        with error ->
          unlock_terminal stdout fmt ; raise error
      end
    else
      Pretty_utils.nullprintf text

  let pp_all_categories () =
    let l = get_all_categories () in
    let max =
      List.fold_left (fun m s -> max m (String.length s)) 0 l
    in
    let print_one_elt fmt s =
      Format.fprintf fmt "%-*s : %s" max s (get_category_help s)
    in
    (* level 0 just in case user asks to display all categories
       in an otherwise quiet run *)
    feedback ~level:0 "@[<v 2>Message categories for %s are:@;%a@]"
      label Format.(pp_print_list ~pp_sep:pp_print_cut print_one_elt) l

  let pp_all_warn_categories_status () =
    let l = get_all_warn_categories_status () in
    let (max, max_status), l =
      (* We need the length of statuses, so we convert them to strings. *)
      List.fold_left_map (fun (m, m') (s, status) ->
          let status = Format.asprintf "%a" pp_warn_status status in
          let max_s = max m (String.length s) in
          let max_status = max m' (String.length status) in
          (max_s, max_status), (s, status)
        ) (0,0) l
    in
    let print_one_elt fmt (s, status) =
      Format.fprintf fmt "%-*s : %-*s : %s" max s max_status status
        (get_warn_category_help s)
    in
    feedback ~level:0 "@[<v 2>Warning categories for %s are@;%a@]"
      label Format.(pp_print_list ~pp_sep:pp_print_cut print_one_elt) l

end

(* Deprecated *)

let kernel_channel_name = "kernel"
let kernel_label_name = "kernel"

let cmdline_error_occurred = Extlib.mk_fun "Log.cmdline_error_occurred"
let cmdline_at_error_exit = Extlib.mk_fun "Log.at_error_exit"

(* ------------------------------------------------------------------------- *)
(* --- Tests                                                             --- *)
(* ------------------------------------------------------------------------- *)

(* Only used in inline tests removed in release mode. *)
let _test_terminal () =
  let buffer = Buffer.create 13 in
  let fmt = Format.formatter_of_buffer buffer in
  Format.pp_set_mark_tags fmt true;
  let validate_result expected =
    Format.pp_print_flush fmt ();
    let result = Buffer.contents buffer in
    let success = result = expected in
    if not success then
      Format.eprintf "wrong output: %S given, %S expected@."
        result expected;
    success
  in
  let channel = new_channel "test" in
  set_terminal channel.terminal true fmt;
  channel, validate_result

let%test _ =
  let channel, validate = _test_terminal () in
  logtransient channel "abcd";
  logtransient channel "abc";
  validate "<bold>[test]</bold> abcd\r\027[K<bold>[test]</bold> abc"

let%test _ =
  let channel, validate = _test_terminal () in
  logtransient channel "abc\ndef";
  validate "<bold>[test]</bold> abc…"

let%test _ =
  let channel, validate = _test_terminal () in
  logtransient channel "@{<a>  abc\ndef@}";
  validate "<bold>[test]</bold> <a>abc…</a>"

let%test _ =
  let channel, validate = _test_terminal () in
  logtransient channel "  abc\n@{<a>@}def";
  validate "<bold>[test]</bold> abc…"
