(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* -------------------------------------------------------------------------- *)
(* --- Socket Server Options                                              --- *)
(* -------------------------------------------------------------------------- *)

module Senv = Server_parameters

let dkey = Senv.dkey_protocol

let socket_group = Senv.add_group "Sockets"

let () = Parameter_customize.is_not_reconfigurable ()
let () = Parameter_customize.set_group socket_group
module Socket = Senv.String
    (struct
      let option_name = "-server-socket"
      let arg_name = "url"
      let default = ""
      let help =
        "Launch the Socket server (in background).\n\
         The server can handle GET requests during the\n\
         execution of the frama-c command line.\n\
         Finally, the server is executed until shutdown.\n\
         For the Internet socket domain, the default url is '127.0.0.1'."
    end)

let () = Parameter_customize.set_group socket_group
module SocketSize = Senv.Int
    (struct
      let option_name = "-server-socket-size"
      let arg_name = "n"
      let default = 256
      let help = "Control the size of socket buffers (in ko, default 256)."
    end)

let () = Parameter_customize.set_group socket_group
module SocketDomain = Senv.String
    (struct
      let option_name = "-server-socket-domain"
      let arg_name = "unix|internet"
      let default = "unix"
      let help = "Socket domain to be used: 'unix' or 'internet'.\n\
                  'unix' is faster, but does not work on Windows\n\
                  nor between different machines."
    end)
let () = SocketDomain.set_possible_values ["unix"; "internet"]

let () = Parameter_customize.set_group socket_group
module SocketPort = Senv.Int
    (struct
      let option_name = "-server-socket-port"
      let arg_name = "n"
      let default = 9225
      let help = "Socket port to be used, if the domain\n\
                  (-server-socket-domain) is 'internet'.\n\
                  The default port is 9225 (9-22-5, I-V-E)."
    end)
let () = SocketPort.set_range ~min:1 ~max:65535

let _ = Server_doc.protocol
    ~title:"Socket Protocol"
    ~readme:"server_socket.md"

(* -------------------------------------------------------------------------- *)
(* --- Socket Bytes Read & Write                                          --- *)
(* -------------------------------------------------------------------------- *)

type channel = {
  sock : Unix.file_descr ; (* Socket *)
  snd  : bytes ; (* SND bytes buffer, re-used for transport *)
  rcv  : bytes ; (* RCV bytes buffer, re-used for transport *)
  brcv : Buffer.t ; (* RCV data buffer, accumulated *)
  bsnd : Buffer.t ; (* SND data buffer, accumulated *)
}

let read_bytes { sock ; rcv ; brcv } =
  (* rcv buffer is only used locally *)
  let s = Bytes.length rcv in
  let rec scan p =
    (* try to fill RCV buffer *)
    let n =
      try Unix.read sock rcv p (s-p)
      with Unix.Unix_error((EAGAIN|EWOULDBLOCK),_,_) -> 0
    in
    let p = p + n in
    if n > 0 && p < s then scan p else p
  in
  let n = scan 0 in
  if n > 0 then Buffer.add_subbytes brcv rcv 0 n

let send_bytes { sock ; snd ; bsnd } =
  (* snd buffer is only used locally *)
  let n = Buffer.length bsnd in
  if n > 0 then
    let s = Bytes.length snd in
    let rec send p =
      (* try to flush BSND buffer *)
      let w = min (n-p) s in
      Buffer.blit bsnd p snd 0 w ;
      let r =
        try Unix.single_write sock snd 0 w
        with Unix.Unix_error((EAGAIN|EWOULDBLOCK),_,_) -> 0
      in
      let p = p + r in
      if r > 0 && p < n then send p else p
    in
    let p = send 0 in
    if p > 0 then
      let tail = Buffer.sub bsnd p (n-p) in
      Buffer.reset bsnd ;
      Buffer.add_string bsnd tail

(* -------------------------------------------------------------------------- *)
(* --- Data Chunks Encoding                                               --- *)
(* -------------------------------------------------------------------------- *)

let read_data ch =
  try
    (* Try to read all the data.
       In case there is not enough bytes in the buffer,
       calls to Buffer.sub would raise Invalid_argument. *)
    let h = match Buffer.nth ch.brcv 0 with
      | 'S' -> 3
      | 'L' -> 7
      | 'W' -> 15
      | _ -> raise (Invalid_argument "Server_socket.read")
    in
    let hex = Buffer.sub ch.brcv 1 h in
    let len = int_of_string ("0x" ^ hex) in
    let data = Buffer.sub ch.brcv (1+h) len in
    let p = 1 + h + len in
    let n = Buffer.length ch.brcv - p in
    (* TODO[LC]: inefficient move. Requires a ring-buffer. *)
    let rest = Buffer.sub ch.brcv p n in
    Buffer.reset ch.brcv ;
    Buffer.add_string ch.brcv rest ;
    Some data
  with Invalid_argument _ -> None

let write_data ch data =
  begin
    let len = String.length data in
    let hex =
      if len < 0xFFF then Printf.sprintf "S%03x" len else
      if len < 0xFFFFFFF then Printf.sprintf "L%07x" len else
        Printf.sprintf "W%015x" len
    in
    Buffer.add_string ch.bsnd hex ;
    Buffer.add_string ch.bsnd data ;
  end

(* -------------------------------------------------------------------------- *)
(* --- Request Encoding                                                   --- *)
(* -------------------------------------------------------------------------- *)

let jfield fd js = Json.field fd js |> Json.string

let decode (data : string) : string Main.request =
  match data with
  | "\"POLL\"" -> `Poll
  | "\"SHUTDOWN\"" -> `Shutdown
  | _ ->
    let js = Yojson.Basic.from_string data in
    match jfield "cmd" js with
    | "GET" | "SET" | "EXEC" ->
      let id = jfield "id" js in
      let request = jfield "request" js in
      let data = Json.field "data" js in
      `Request(id,request,data)
    | "SIGON" -> `SigOn (jfield "id" js)
    | "SIGOFF" -> `SigOff (jfield "id" js)
    | "KILL" -> `Kill (jfield "id" js)
    | _ ->
      Senv.feedback ~dkey
        "Invalid socket command:@ @[<hov 2>%a@]"
        Json.pp js ;
      raise Not_found

let encode (resp : string Main.response) : string =
  let js =
    match resp with
    | `Data(id,data) -> `Assoc [
        "res", `String "DATA" ;
        "id", `String id ;
        "data", data ]
    | `Error(id,msg) -> `Assoc [
        "res", `String "ERROR" ;
        "id", `String id ;
        "msg", `String msg ]
    | `Killed id -> `Assoc [
        "res", `String "KILLED" ;
        "id", `String id ]
    | `Rejected id -> `Assoc [
        "res", `String "REJECTED" ;
        "id", `String id ]
    | `Signal id -> `Assoc [
        "res", `String "SIGNAL" ;
        "id", `String id ]
    | `CmdLineOn -> `String "CMDLINEON"
    | `CmdLineOff -> `String "CMDLINEOFF"
  in Yojson.Basic.to_string ~std:false js

let parse ch =
  let rec scan cmds ch =
    match read_data ch with
    | None -> List.rev cmds
    | Some data ->
      match decode data with
      | cmd -> scan (cmd::cmds) ch
      | exception _ -> scan cmds ch
  in scan [] ch

(* -------------------------------------------------------------------------- *)
(* --- Socket Messages                                                    --- *)
(* -------------------------------------------------------------------------- *)

let callback ch rs =
  List.iter
    (fun r ->
       match encode r with
       | data -> write_data ch data
       | exception err ->
         Senv.feedback ~dkey "Socket: encoding error %S@."
           (Printexc.to_string err)
    ) rs ;
  send_bytes ch

let commands ch =
  begin
    read_bytes ch ;
    match parse ch with
    | [] -> send_bytes ch ; None
    | requests -> Some Main.{ requests ; callback = callback ch }
  end

(* -------------------------------------------------------------------------- *)
(* --- Establish the Server                                               --- *)
(* -------------------------------------------------------------------------- *)

type socket = {
  socket : Unix.file_descr ;
  mutable channel : channel option ;
}

let close (s: socket) =
  match s.channel with None -> () | Some ch ->
    s.channel <- None ;
    Unix.close ch.sock

let set_socket_size sock opt s =
  begin
    let nbytes = s * 1024 in
    (try Unix.setsockopt_int sock opt nbytes
     with Unix.Unix_error(err,_,_) ->
       let msg = Unix.error_message err in
       Senv.warning ~once:true
         "Invalid socket size (%d: %s)" nbytes msg) ;
    Unix.getsockopt_int sock opt
  end

let channel (s: socket) =
  match s.channel with
  | Some _ as chan -> chan
  | None ->
    try
      let sock,_ = Unix.accept ~cloexec:true s.socket in
      Unix.set_nonblock sock ;
      let size = SocketSize.get () in
      let rcv = set_socket_size sock SO_RCVBUF size in
      let snd = set_socket_size sock SO_SNDBUF size in
      Senv.feedback ~dkey ~level:2 "Socket size in:%d out:%d@." rcv snd ;
      Senv.debug "Client connected" ;
      let ch = Some {
          sock ;
          snd = Bytes.create snd ;
          rcv = Bytes.create rcv ;
          bsnd = Buffer.create snd ;
          brcv = Buffer.create rcv ;
        } in
      s.channel <- ch ; ch
    with Unix.Unix_error(EAGAIN,_,_) -> None

let fetch (s:socket) () =
  try match channel s with
    | None -> None
    | Some ch -> commands ch
  with
  | Unix.Unix_error(EPIPE,_,_) ->
    Senv.debug "Client disconnected" ;
    close s ; None
  | exn ->
    Senv.warning "Socket: exn %s" (Printexc.to_string exn) ;
    close s ; None

let establish_server fd =
  let socket = { socket = fd ; channel = None } in
  try
    Unix.listen fd 1 ;
    Senv.feedback ~dkey "Socket server: listening up to 1 pending request";
    begin
      try
        ignore (Sys.signal Sys.sigpipe Signal_ignore) ;
      with _ -> Senv.debug "SIGPIPE unsupported in this OS, ignoring"
    end;
    let pretty = Format.pp_print_string in
    let server = Main.create ~pretty ~fetch:(fetch socket) () in
    Extlib.safe_at_exit
      begin fun () ->
        Main.stop server ;
        close socket ;
      end ;
    Main.start server ;
    Cmdline.at_normal_exit
      begin fun () ->
        Main.run server ;
        close socket ;
      end;
  with exn ->
    close socket ;
    raise exn

(* -------------------------------------------------------------------------- *)
(* --- Synchronous Server                                                 --- *)
(* -------------------------------------------------------------------------- *)

(* address of the currently-running server, if any *)
let server_addr = ref None

(* port of the currently-running server, if any (Internet domain only) *)
let server_port = ref None

let cmdline () =
  match Socket.get () with
  | "" -> ()
  | addr ->
    match (SocketDomain.get ()) with
    | "unix" ->
      begin
        match !server_addr with
        | Some addr0 ->
          begin
            if addr0 <> addr then
              Senv.warning "Socket server already running on [%s]." addr0
          end
        | None ->
          try
            server_addr := Some addr;
            let addr_path = Filepath.of_string addr in
            if Filesystem.exists addr_path then Unix.unlink addr ;
            let fd = Unix.socket PF_UNIX SOCK_STREAM 0 in
            Unix.bind fd (ADDR_UNIX addr) ;
            if Senv.is_debug_key_enabled dkey then
              Senv.feedback ~dkey "Socket server running on [%s]." addr
            else
              Senv.feedback "Socket server running." ;
            establish_server fd ;
          with exn ->
            Senv.fatal "Server Unix socket failed.@\nError: %s"
              (Printexc.to_string exn)
      end
    | "internet" ->
      begin
        let addr = if addr = "" then "127.0.0.1" else addr in
        let port = SocketPort.get () in
        match !server_addr, !server_port with
        | Some addr0, Some port0 ->
          begin
            if addr0 <> addr || port0 <> port then
              Senv.warning "Socket server already running on [%s:%d]." addr0 port0
          end
        | _ ->
          try
            server_addr := Some addr ;
            server_port := Some port ;
            let fd = Unix.socket PF_INET SOCK_STREAM 0 in
            let addr_inet = Unix.inet_addr_of_string addr in
            Senv.feedback ~dkey "Socket server will bind on %s:%d..." addr port;
            Unix.bind fd (Unix.ADDR_INET (addr_inet, port)) ;
            if Senv.is_debug_key_enabled dkey then
              Senv.feedback ~dkey "Socket server running on [%s:%d]." addr port
            else
              Senv.feedback "Socket server running.";
            establish_server fd ;
          with exn ->
          match exn with
          | Unix.Unix_error (Unix.EADDRINUSE, "bind", _) ->
            (* Note: this does not happen when SO_REUSEPORT is used *)
            Senv.abort "bind failed: EADDRINUSE. Terminate all previous \
                        Frama-C server processes and restart the GUI."
          | _ ->
            Senv.fatal "Server Internet socket failed.@\nError: %s"
              (Printexc.to_string exn)
      end
    | _ -> assert false

let () = Boot.Main.extend cmdline

(* -------------------------------------------------------------------------- *)
