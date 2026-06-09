open Cil_datatype

let loc = Fileloc.unknown

let null () =
  let e = Cil.zero ~loc in
  Cil.mkCast ~force:true ~newt:Cil_const.voidPtrType e

let inull () =
  let e = Cil.zero ~loc in
  Cil.mkCast ~force:true ~newt:Cil_const.intPtrType e

let cone () =
  let e = Cil.one ~loc in
  Cil.mkCast ~force:true ~newt:Cil_const.charPtrType e

let ione () =
  let e = Cil.one ~loc in
  Cil.mkCast ~force:true ~newt:Cil_const.intPtrType e

let test =
  let n = ref 0 in
  fun e1 e2 ->
    incr n;
    let e = Cil.mkBinOp_exn ~loc Cil_types.Eq (e1 ()) (e2 ()) in
    Format.printf "TEST %d: %a@." !n Exp.pretty e

let main () =
  test null null;
  test null inull;
  test inull null;

  test null cone;
  test cone null;
  test cone cone;

  test cone ione;
  test ione cone

let () = Boot.Main.extend main
