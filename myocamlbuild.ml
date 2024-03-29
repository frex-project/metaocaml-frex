open Ocamlbuild_plugin;;
open Ocamlbuild_pack;;

dispatch begin
  function
  | After_rules ->
     begin match Sys.ocaml_version with
     | "4.02.1+dev0-2014-08-29" | "4.04.0" ->
        flag ["ocaml"; "compile"; "open_print_code"] & S[A"-open"; A"Print_code"]
     | "4.07.1" | "4.11.1" | "4.14.1" ->
        flag ["ocaml"; "compile"; "open_print_code"] & S[A"-open"; A"Codelib"]
     | version ->
        Printf.kprintf failwith "Unsupported OCaml version %s" version
     end
  | _ -> ()
end;;

