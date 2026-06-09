let () = Plugin.is_session_visible ()
module Self =
  Plugin.Register
    (struct
      let name = "Directories"
      let shortname = "dirs"
      let help = ""
    end)

module OnlyCache = Self.False(struct
    let option_name = "-dirs-cache-only"
    let help = ""
  end)

module Cache = Self.Cache_dir ()
module Sub_cache_no_opt =
  Self.Make_user_dir
    (Cache)
    (struct let name = "noopt" end)

module Sub_cache_opt_no_var =
  Self.Make_user_dir_opt
    (Cache)
    (struct
      let option_name = "-dirs-opt-no-var"
      let arg_name = "dir"
      let help = ""
      let env = None
      let dirname = "optnovar"
    end)

module Sub_cache_opt_var =
  Self.Make_user_dir_opt
    (Cache)
    (struct
      let option_name = "-dirs-optvar"
      let arg_name = "dir"
      let help = ""
      let env = Some "FRAMAC_DIRS_VAR"
      let dirname = "optvar"
    end)

module Config = Self.Config_dir ()
module State = Self.State_dir ()
module Session = Self.Session

let never_fail_get f x =
  try
    let s = f x in
    Self.feedback "Found: %a" Filepath.pretty s
  with _ -> ()

let run_all () =
  if OnlyCache.get ()
  then begin
    ignore @@ Cache.get_dir ~create_path:true "created" ;
    ignore @@ Sub_cache_no_opt.get_dir ~create_path:true "." ;
    ignore @@ Sub_cache_no_opt.get_dir "foo" ;
    ignore @@ Sub_cache_opt_no_var.get_dir ~create_path:true "." ;
    ignore @@ Sub_cache_opt_no_var.get_dir "foo" ;
    ignore @@ Sub_cache_opt_var.get_dir ~create_path:true "." ;
    ignore @@ Sub_cache_opt_var.get_dir "foo" ;

    ignore @@ Sub_cache_no_opt.get_file "file" ;
    ignore @@ Sub_cache_opt_var.get_file "file" ;

    never_fail_get Cache.get_file "created" ;
    never_fail_get Sub_cache_opt_var.get_file "."
  end
  else
    try
      ignore @@ Cache.get_dir ~create_path:true "created" ;
      ignore @@ Config.get_dir ~create_path:true "created" ;
      ignore @@ State.get_dir ~create_path:true "created" ;
      ignore @@ Session.get_dir ~create_path:true "created" ;
      ignore @@ Session.get_file ~create_path:true "created_filepath/file" ;

      let cache_dir = Cache.get_dir "not_created" in
      let config_dir = Config.get_dir "not_created" in
      let state_dir = State.get_dir "not_created" in
      let session_dir = Session.get_dir "not_created" in
      let session_file = Session.get_file "not_created_filepath/file" in

      Self.feedback "Not created:" ;
      Self.feedback "%a" Filepath.pretty cache_dir ;
      Self.feedback "%a" Filepath.pretty config_dir ;
      Self.feedback "%a" Filepath.pretty state_dir ;
      Self.feedback "%a" Filepath.pretty session_dir ;
      Self.feedback "%a" Filepath.pretty session_file
    with Not_found ->
      Self.error "Failure when creating directories"

let () = Boot.Main.extend run_all
