let pp_dec fmt z = Z.pretty fmt z
let pp_hex fmt z = Z.pp_hex ~nbits:16 ~sep:"_" fmt z
let pp_bin fmt z = Z.pp_bin ~nbits:8  ~sep:"_" fmt z

let hrule () =
  Format.printf "--------------------------------------------------@."

let testcase z =
  begin
    hrule () ;
    Format.printf "Dec. %a@." pp_dec z ;
    Format.printf "Hex. %a@." pp_hex z ;
    Format.printf "Bin. %a@." pp_bin z ;
  end

let () =
  begin
    List.iter
      (fun z ->
         testcase z ;
         if not (Z.is_zero z) then
           testcase (Z.neg z)
      ) [
      Z.of_string "0" ;
      Z.of_string "1" ;
      Z.of_string "2" ;
      Z.of_string "5" ;
      Z.of_string "9" ;
      Z.of_string "16" ;
      Z.of_string "127" ;
      Z.of_string "128" ;
      Z.of_string "0xFF" ;
      Z.of_string "0xFF0F000F" ;
      Z.of_string "0x17070007" ;
    ] ;
    hrule () ;
  end
