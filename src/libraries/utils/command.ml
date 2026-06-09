(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* -------------------------------------------------------------------------- *)
(* --- Pretty from files                                                  --- *)
(* -------------------------------------------------------------------------- *)

let pp_from_file fmt path =
  let open Filesystem.Operators in
  let$ cin = Filesystem.with_open_in_exn path in
  try
    while true do
      Async.yield () ;
      let line = input_line cin in
      Format.pp_print_string fmt line ;
      Format.pp_print_newline fmt () ;
    done
  with
  | End_of_file -> ()

(* -------------------------------------------------------------------------- *)
(* --- Timing                                                             --- *)
(* -------------------------------------------------------------------------- *)

type timer = float ref
let dt_max tm dt = match tm with Some r when dt > !r -> r := dt | _ -> ()
let dt_add tm dt = match tm with Some r -> r := !r +. dt | _ -> ()
let time ?rmax ?radd job data =
  let t0 = Sys.time () in
  try
    let result = job data in
    let t1 = Sys.time () in
    let dt = t1 -. t0 in
    dt_max rmax dt ;
    dt_add radd dt ;
    result
  with exn ->
    let t1 = Sys.time () in
    let dt = t1 -. t0 in
    dt_max rmax dt ;
    dt_add radd dt ;
    raise exn

(* -------------------------------------------------------------------------- *)
(* --- Process                                                            --- *)
(* -------------------------------------------------------------------------- *)

type process_result = Not_ready of (unit -> unit) | Result of Unix.process_status

let _pp_status fmt = function
  | Unix.WEXITED s -> Format.fprintf fmt "exit[%d]" s
  | Unix.WSIGNALED s -> Format.fprintf fmt "sig[%d]" s
  | Unix.WSTOPPED s -> Format.fprintf fmt "stop[%d]" s

let flush b f =
  match b with
  | None -> ()
  | Some b ->
    try Filesystem.iter_lines f
          (fun line -> Buffer.add_string b line ; Buffer.add_char b '\n') ;
    with Sys_error _ -> ()

(*[LC] return the cancel function *)
let cancelable_at_exit job =
  let later = ref (Some job) in
  Extlib.safe_at_exit
    (fun () -> match !later with None -> () | Some job -> job ()) ;
  fun () -> later := None

let command_generic ~async ?stdout ?stderr cmd args =
  let inf,inc = Filename.open_temp_file
      ~mode:[Open_binary;Open_rdonly; Open_trunc; Open_creat; Open_nonblock ]
      "in_" ".tmp"
  in
  let outf,outc = Filename.open_temp_file
      ~mode:[Open_binary;Open_wronly; Open_trunc; Open_creat]
      "out_" ".tmp"
  in
  let errf,errc = Filename.open_temp_file
      ~mode:[Open_binary;Open_wronly; Open_trunc; Open_creat]
      "out_" ".tmp"
  in
  let delete () =
    begin
      Filesystem.remove_file (Filepath.of_string inf);
      Filesystem.remove_file (Filepath.of_string outf);
      Filesystem.remove_file (Filepath.of_string errf);
    end in
  let deleted = cancelable_at_exit delete in
  let pid = Unix.create_process cmd (Array.of_list (cmd :: args))
      (Unix.descr_of_out_channel inc)
      (Unix.descr_of_out_channel outc)
      (Unix.descr_of_out_channel errc)
  in
  let killed = cancelable_at_exit
      begin fun () ->
        Unix.kill pid Sys.sigkill;
        Unix.(try ignore (waitpid [] pid) with Unix_error _ -> ()) ;
      end in
  close_out_noerr inc;
  close_out_noerr outc;
  close_out_noerr errc;
  let kill () = Unix.kill pid Sys.sigkill in
  let last_result= ref (Not_ready kill) in
  let wait_flags =
    if async
    then [Unix.WNOHANG; Unix.WUNTRACED]
    else [Unix.WUNTRACED]
  in
  begin fun () ->
    match !last_result with
    | Result _p as r -> r
    | Not_ready _ as r ->
      let child_id,status = Unix.waitpid wait_flags pid in
      if child_id = 0 then (assert async;r)
      else
        begin
          let result = Result status in
          flush stdout (Filepath.of_string outf) ;
          flush stderr (Filepath.of_string errf) ;
          delete () ;
          deleted () ;
          killed () ;
          result
        end
  end

let async ?stdout ?stderr cmd args =
  command_generic ~async:true ?stdout ?stderr cmd args

let spawn ~async ?(timeout=0) ?stdout ?stderr cmd args =
  if async || timeout > 0 then
    let f = command_generic ~async:true ?stdout ?stderr cmd args in
    let res = ref(Unix.WEXITED 99) in
    let ftimeout = float_of_int timeout in
    let start = ref (Unix.gettimeofday ()) in
    let running () =
      match f () with
      | Not_ready terminate ->
        begin
          try
            Async.yield () ;
            if timeout > 0 && Unix.gettimeofday () -. !start > ftimeout then
              raise Async.Cancel ;
            true
          with Async.Cancel as e ->
            terminate ();
            raise e
        end
      | Result r ->
        res := r;
        false
    in while running () do Unix.sleepf 0.1 done ; !res
  else
    let f = command_generic ~async:false ?stdout ?stderr cmd args in
    match f () with
    | Result r -> r
    | Not_ready _ -> assert false

module Dot =
struct
  type format = Jpeg | Pdf | Png | Svg

  let format_to_string = function
    | Jpeg -> "jpeg"
    | Pdf -> "pdf"
    | Png -> "png"
    | Svg -> "svg"

  let spawn ~async ?timeout ?layout ~format ~output input =
    let layout_args = match layout with
      | Some s -> ["-K" ; s]
      | None -> []
    in
    let args = layout_args @
               [ "-T" ; format_to_string format ;
                 "-o" ; Filepath.to_string_abs output ;
                 Filepath.to_string_abs input ]
    in
    spawn ~async ?timeout "dot" args
end
