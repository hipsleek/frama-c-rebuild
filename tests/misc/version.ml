let re_version = Str.regexp "^\\([0-9]+\\)\\.\\([0-9]+\\)"

let run () =
  let version_str = System_config.Version.id in
  if Str.string_match re_version version_str 0 then
    let major = Str.matched_group 1 version_str in
    let minor = Str.matched_group 2 version_str in
    if major = string_of_int System_config.Version.major &&
       minor = string_of_int System_config.Version.minor
    then
      Kernel.feedback "version numbers match"
    else
      Kernel.abort
        "error parsing major/minor version: expected %s.%s, got %d.%d"
        major minor System_config.Version.major System_config.Version.minor
  else
    Kernel.abort
      "could not parse System_config.Version.id"

let () = Boot.Main.extend run
