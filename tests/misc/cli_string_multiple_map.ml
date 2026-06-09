include
  Plugin.Register
    (struct
      let name = "A"
      let shortname = "a"
      let help = ""
    end)

module M =
  String_multiple_map
    (struct
      include Z
      type key = string
      let of_string arg =
        try Z.of_string arg
        with Failure _ ->
          raise (Cannot_build "expecting an integer")
      let to_string = Z.to_string
    end)
    (struct
      let option_name = "-multiple-map"
      let help = ""
      let default = Datatype.String.Map.empty
      let arg_name = "s:i"
    end)


let main () =
  let print k v =
    feedback "%s => %a" k (Pretty_utils.pp_list ~sep:";@," Z.pretty) v
  in
  Datatype.String.Map.iter print (M.get ())

let () = Boot.Main.extend main
