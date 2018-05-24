(* A small-scale example illustrating partially-static monoids.

   References for the unstaged printf function:

     Functional Unparsing
     Olivier Danvy
     May 1998 -- JFP 8(6):621-625, 1998, extended version

     On typing delimited continuations: three new solutions to the
     printf problem
     Kenichi Asai
     Higher-Order and Symbolic Computation, 2009

   Reference for a staged "tagged" (GADT-based) printf function:

     Modular macros (extended abstract)
     Jeremy Yallop and Leo White
     OCaml 2015

   The implementation that uses partially-static data is new here.
*)

(** A standard typed printf function based on CPS with an accumulator. *)
module Printf_unstaged :
sig
  type (_,_) t

  val lit : string -> ('a, 'a) t

  val (++) : ('b, 'a) t -> ('c, 'b) t -> ('c, 'a) t

  val int : ('a, int -> 'a) t
  val str : ('a, string -> 'a) t

  val sprintf : (string, 'a) t -> 'a
end =
struct
  type ('a,'r) t = (string -> 'a) -> (string -> 'r)

  let lit x = fun k -> fun s -> k (s ^ x)

  let (++) f1 f2 = fun k -> f1 (f2 k)

  let sprintf p = p (fun s -> s) ""

  let int' x = string_of_int x

  let str' x = x

  let (!%) to_str = fun k -> fun s -> fun x -> k (s ^ to_str x)

  let int x = !% int' x
  let str x = !% str' x
end

(** A staged version of Printf_unstaged -- identical except for
    quoting and splicing annotations and the 'code' type constructor.

    The continuation function is treated as static, and so it doesn't
    appear in the generated code.
*)
module Printf_staged :
sig
  type (_,_) t

  val lit : string -> ('a, 'a) t

  val (++) : ('b, 'a) t -> ('c, 'b) t -> ('c, 'a) t

  val int : ('a, int code -> 'a) t
  val str : ('a, string code -> 'a) t

  val sprintf : (string code, 'a) t -> 'a
end =
struct
  type ('a,'r) t = (string code -> 'a) -> (string code -> 'r)

  let lit x = fun k -> fun s -> k .<.~s ^ x>.

  let (++) f1 f2 = fun k -> f1 (f2 k)

  let sprintf p = p (fun s -> s) .<"">.

  let int' x = .<string_of_int .~x>.
  let str' x = x

  let (!%) to_str = fun k -> fun s -> fun x -> k .<.~s ^ .~(to_str x)>.

  let int x = !% int' x
  let str x = !% str' x
end

(** An improved version of Printf_staged that uses partially-static
    strings rather than dynamic strings internally.  The interface is
    identical.  Using partially-static strings means that adjacent
    literal strings appear already catenated in the generated code,
    even if they are not adjacent in the source:

        (x ++ lit "a") ++ (lit "b" ++ y)
      ~>
        .< ... x ^ "ab" ^ y ... >.

    In this way the partially-static data structure makes use of the
    associativity of the underlying operation to generate more
    efficient code. *)
module Printf_partially_static :
sig
  type (_,_) t

  val lit : string -> ('a, 'a) t

  val (++) : ('b, 'a) t -> ('c, 'b) t -> ('c, 'a) t

  val int : ('a, int code -> 'a) t
  val str : ('a, string code -> 'a) t

  val sprintf : (string code, 'a) t -> 'a
end =
struct
  module String_monoid : Algebra.MONOID with type t = string = struct
    type t = string
    let unit = ""
    let (<*>) = (^)
  end

  module String_code_monoid : Algebra.MONOID with type t = string code = struct
    type t = string code
    let unit = .<"">.
    let (<*>) x y = .< .~x ^ .~y >.
  end

  module Ps_string = Monoids.PS_monoid(String_monoid)(String_code_monoid)
  (* module Ps_monoid = (\* Monoids.MkMONOID *\)(Ps_string) *)

  open Ps_string
  open Ps_string.Op
  (* open Ps_monoid *)

  let rec gen_list : string code list -> string list code = function
    | [] -> .< [] >.
    | x :: xs -> .< .~x :: .~(gen_list xs) >.

  let cd_string x =
    let module E = Eva(struct module Op = struct
                                type t = string code list
                                let unit = []
                                let (<*>) = (@)
                              end
        module T = struct type t = string code list end
      end) in
    let lst x = [Aux.cd_of_var x] in
    let persist (s:string) = [.< s >.] in
    .< String.concat "" .~(gen_list (E.eva persist lst x)) >.

  type ('a,'r) t = (Ps_string.T.t -> 'a) -> (Ps_string.T.t -> 'r)

  let lit x = fun k -> fun s -> k (s <*> sta x)

  let (++) f1 f2 = fun k -> f1 (f2 k)

  let sprintf p = p (fun s -> cd_string s) unit

  let int' x = var (Aux.var .<string_of_int .~x>.)
  let str' x = var (Aux.var x)

  let (!%) to_str = fun k -> fun s -> fun x -> k (s <*> to_str x)

  let int x = !% int' x
  let str x = !% str' x
end


let unstaged_example : int -> int -> string =
  Printf_unstaged.(sprintf
                     (lit "(" ++ int ++ lit "," ++ int ++ lit ")"))

let staged_example : int code -> int code -> string code =
  Printf_staged.(sprintf
                     (lit "(" ++ int ++ lit "," ++ int ++ lit ")"))

let partially_static_example : int code -> int code -> string code =
  Printf_partially_static.(sprintf
                             (lit "(" ++ int ++ lit "," ++ int ++ lit ")"))

(* open Core.Std *)
(* open Core_bench.Std *)

(* let args = [0;1;2;3;4;5;6;7;8;9] *)
(* let functions = [ *)
(*   let spr = Sys.opaque_identity (sprintf (lit "")) in *)
(*   Staged.stage *)
(*     (fun () -> ignore spr); *)
  
(*   let spr = Sys.opaque_identity (sprintf ("(" ++ str ++ ")")) in *)
(*   Staged.stage *)
(*     (fun () -> ignore (spr "1")); *)

(*   let spr = Sys.opaque_identity (sprintf ("(" ++ str ++ ")") *)
(*                                  ++      ("(" ++ str ++ ")")) in *)
(*   Staged.stage *)
(*     (fun () -> ignore (spr "1" "2")); *)


(*   let spr = Sys.opaque_identity (sprintf ("(" ++ str ++ ")") *)
(*                                  ++      ("(" ++ str ++ ")") *)
(*                                  ++      ("(" ++ str ++ ")")) in *)
(*   Staged.stage *)
(*     (fun () -> ignore (spr "1" "2" "3")); *)

(*   let spr = Sys.opaque_identity (sprintf ("(" ++ str ++ ")") *)
(*                                  ++      ("(" ++ str ++ ")") *)
(*                                  ++      ("(" ++ str ++ ")") *)
(*                                  ++      ("(" ++ str ++ ")")) in *)
(*   Staged.stage *)
(*     (fun () -> ignore (spr "1" "2" "3" "4")); *)
  
(* ] *)

let () = begin
  Printf.printf "staged\n\n" ;

  Format.fprintf Format.std_formatter "let pr0 = %a@\n"
    Print_code.print_code 
    .< .~(Printf_staged.(sprintf (lit ""))) >.
  ;

  Format.fprintf Format.std_formatter "let pr1 = %a@\n"
    Print_code.print_code 
    .< fun x1 -> 
      .~(Printf_staged.(sprintf
                          (lit "(" ++ str ++ lit ")")
                          .<x1>.)) >.
  ;

  Format.fprintf Format.std_formatter "let pr2 = %a@\n"
    Print_code.print_code
    .< fun x1 x2 -> 
      .~(Printf_staged.(sprintf
                          ((lit "(" ++ str ++ lit ")")
                           ++
                           (lit "(" ++ str ++ lit ")"))
                           .<x1>.
                           .<x2>.)) >.
  ;

  Format.fprintf Format.std_formatter "let pr3 = %a@\n"
    Print_code.print_code
    .< fun x1 x2 x3 -> 
      .~(Printf_staged.(sprintf
                          ((lit "(" ++ str ++ lit ")")
                           ++
                           (lit "(" ++ str ++ lit ")")
                           ++
                           (lit "(" ++ str ++ lit ")"))
                           .<x1>.
                           .<x2>.
                           .<x3>.)) >.
  ;

  Format.fprintf Format.std_formatter "let pr4 = %a@\n"
    Print_code.print_code
    .< fun x1 x2 x3 x4 -> 
      .~(Printf_staged.(sprintf
                          ((lit "(" ++ str ++ lit ")")
                           ++
                           (lit "(" ++ str ++ lit ")")
                           ++
                           (lit "(" ++ str ++ lit ")")
                           ++
                           (lit "(" ++ str ++ lit ")"))
                           .<x1>.
                           .<x2>.
                           .<x3>.
                           .<x4>.)) >.
  ;

  Format.fprintf Format.std_formatter "let pr5 = %a@\n"
    Print_code.print_code
    .< fun x1 x2 x3 x4 x5 -> 
      .~(Printf_staged.(sprintf
                          ((lit "(" ++ str ++ lit ")")
                           ++
                           (lit "(" ++ str ++ lit ")")
                           ++
                           (lit "(" ++ str ++ lit ")")
                           ++
                           (lit "(" ++ str ++ lit ")")
                           ++
                           (lit "(" ++ str ++ lit ")"))
                           .<x1>.
                           .<x2>.
                           .<x3>.
                           .<x4>.
                           .<x5>.)) >.
  ;

  Format.fprintf Format.std_formatter "let pr6 = %a@\n"
    Print_code.print_code
    .< fun x1 x2 x3 x4 x5 x6 -> 
      .~(Printf_staged.(sprintf
                          ((lit "(" ++ str ++ lit ")")
                           ++
                           (lit "(" ++ str ++ lit ")")
                           ++
                           (lit "(" ++ str ++ lit ")")
                           ++
                           (lit "(" ++ str ++ lit ")")
                           ++
                           (lit "(" ++ str ++ lit ")")
                           ++
                           (lit "(" ++ str ++ lit ")"))
                           .<x1>.
                           .<x2>.
                           .<x3>.
                           .<x4>.
                           .<x5>.
                           .<x6>.)) >.
  ;

  Format.fprintf Format.std_formatter "let pr7 = %a@\n"
    Print_code.print_code
    .< fun x1 x2 x3 x4 x5 x6 x7 -> 
      .~(Printf_staged.(sprintf
                          ((lit "(" ++ str ++ lit ")")
                           ++
                           (lit "(" ++ str ++ lit ")")
                           ++
                           (lit "(" ++ str ++ lit ")")
                           ++
                           (lit "(" ++ str ++ lit ")")
                           ++
                           (lit "(" ++ str ++ lit ")")
                           ++
                           (lit "(" ++ str ++ lit ")")
                           ++
                           (lit "(" ++ str ++ lit ")"))
                           .<x1>.
                           .<x2>.
                           .<x3>.
                           .<x4>.
                           .<x5>.
                           .<x6>.
                           .<x7>.)) >.
  ;

  Format.fprintf Format.std_formatter "let pr8 = %a@\n"
    Print_code.print_code
    .< fun x1 x2 x3 x4 x5 x6 x7 x8 -> 
      .~(Printf_staged.(sprintf
                          ((lit "(" ++ str ++ lit ")")
                           ++
                           (lit "(" ++ str ++ lit ")")
                           ++
                           (lit "(" ++ str ++ lit ")")
                           ++
                           (lit "(" ++ str ++ lit ")")
                           ++
                           (lit "(" ++ str ++ lit ")")
                           ++
                           (lit "(" ++ str ++ lit ")")
                           ++
                           (lit "(" ++ str ++ lit ")")
                           ++
                           (lit "(" ++ str ++ lit ")"))
                           .<x1>.
                           .<x2>.
                           .<x3>.
                           .<x4>.
                           .<x5>.
                           .<x6>.
                           .<x7>.
                           .<x8>.)) >.
  ;

  Format.fprintf Format.std_formatter "let pr9 = %a@\n"
    Print_code.print_code
    .< fun x1 x2 x3 x4 x5 x6 x7 x8 x9 -> 
      .~(Printf_staged.(sprintf
                          ((lit "(" ++ str ++ lit ")")
                           ++
                           (lit "(" ++ str ++ lit ")")
                           ++
                           (lit "(" ++ str ++ lit ")")
                           ++
                           (lit "(" ++ str ++ lit ")")
                           ++
                           (lit "(" ++ str ++ lit ")")
                           ++
                           (lit "(" ++ str ++ lit ")")
                           ++
                           (lit "(" ++ str ++ lit ")")
                           ++
                           (lit "(" ++ str ++ lit ")")
                           ++
                           (lit "(" ++ str ++ lit ")"))
                           .<x1>.
                           .<x2>.
                           .<x3>.
                           .<x4>.
                           .<x5>.
                           .<x6>.
                           .<x7>.
                           .<x8>.
                           .<x9>.)) >.
  ;




  Printf.printf "staged + partially-static\n\n" ;

  Format.fprintf Format.std_formatter "let ps0 = %a@\n"
    Print_code.print_code 
    .< .~(Printf_partially_static.(sprintf (lit ""))) >.
  ;

  Format.fprintf Format.std_formatter "let ps1 = %a@\n"
    Print_code.print_code 
    .< fun x1 -> 
      .~(Printf_partially_static.(sprintf
                          (lit "(" ++ str ++ lit ")")
                          .<x1>.)) >.
  ;

  Format.fprintf Format.std_formatter "let ps2 = %a@\n"
    Print_code.print_code
    .< fun x1 x2 -> 
      .~(Printf_partially_static.(sprintf
                          ((lit "(" ++ str ++ lit ")")
                           ++
                           (lit "(" ++ str ++ lit ")"))
                           .<x1>.
                           .<x2>.)) >.
  ;

  Format.fprintf Format.std_formatter "let ps3 = %a@\n"
    Print_code.print_code
    .< fun x1 x2 x3 -> 
      .~(Printf_partially_static.(sprintf
                          ((lit "(" ++ str ++ lit ")")
                           ++
                           (lit "(" ++ str ++ lit ")")
                           ++
                           (lit "(" ++ str ++ lit ")"))
                           .<x1>.
                           .<x2>.
                           .<x3>.)) >.
  ;

  Format.fprintf Format.std_formatter "let ps4 = %a@\n"
    Print_code.print_code
    .< fun x1 x2 x3 x4 -> 
      .~(Printf_partially_static.(sprintf
                          ((lit "(" ++ str ++ lit ")")
                           ++
                           (lit "(" ++ str ++ lit ")")
                           ++
                           (lit "(" ++ str ++ lit ")")
                           ++
                           (lit "(" ++ str ++ lit ")"))
                           .<x1>.
                           .<x2>.
                           .<x3>.
                           .<x4>.)) >.
  ;

  Format.fprintf Format.std_formatter "let ps5 = %a@\n"
    Print_code.print_code
    .< fun x1 x2 x3 x4 x5 -> 
      .~(Printf_partially_static.(sprintf
                          ((lit "(" ++ str ++ lit ")")
                           ++
                           (lit "(" ++ str ++ lit ")")
                           ++
                           (lit "(" ++ str ++ lit ")")
                           ++
                           (lit "(" ++ str ++ lit ")")
                           ++
                           (lit "(" ++ str ++ lit ")"))
                           .<x1>.
                           .<x2>.
                           .<x3>.
                           .<x4>.
                           .<x5>.)) >.
  ;

  Format.fprintf Format.std_formatter "let ps6 = %a@\n"
    Print_code.print_code
    .< fun x1 x2 x3 x4 x5 x6 -> 
      .~(Printf_partially_static.(sprintf
                          ((lit "(" ++ str ++ lit ")")
                           ++
                           (lit "(" ++ str ++ lit ")")
                           ++
                           (lit "(" ++ str ++ lit ")")
                           ++
                           (lit "(" ++ str ++ lit ")")
                           ++
                           (lit "(" ++ str ++ lit ")")
                           ++
                           (lit "(" ++ str ++ lit ")"))
                           .<x1>.
                           .<x2>.
                           .<x3>.
                           .<x4>.
                           .<x5>.
                           .<x6>.)) >.
  ;

  Format.fprintf Format.std_formatter "let ps7 = %a@\n"
    Print_code.print_code
    .< fun x1 x2 x3 x4 x5 x6 x7 -> 
      .~(Printf_partially_static.(sprintf
                          ((lit "(" ++ str ++ lit ")")
                           ++
                           (lit "(" ++ str ++ lit ")")
                           ++
                           (lit "(" ++ str ++ lit ")")
                           ++
                           (lit "(" ++ str ++ lit ")")
                           ++
                           (lit "(" ++ str ++ lit ")")
                           ++
                           (lit "(" ++ str ++ lit ")")
                           ++
                           (lit "(" ++ str ++ lit ")"))
                           .<x1>.
                           .<x2>.
                           .<x3>.
                           .<x4>.
                           .<x5>.
                           .<x6>.
                           .<x7>.)) >.
  ;

  Format.fprintf Format.std_formatter "let ps8 = %a@\n"
    Print_code.print_code
    .< fun x1 x2 x3 x4 x5 x6 x7 x8 -> 
      .~(Printf_partially_static.(sprintf
                          ((lit "(" ++ str ++ lit ")")
                           ++
                           (lit "(" ++ str ++ lit ")")
                           ++
                           (lit "(" ++ str ++ lit ")")
                           ++
                           (lit "(" ++ str ++ lit ")")
                           ++
                           (lit "(" ++ str ++ lit ")")
                           ++
                           (lit "(" ++ str ++ lit ")")
                           ++
                           (lit "(" ++ str ++ lit ")")
                           ++
                           (lit "(" ++ str ++ lit ")"))
                           .<x1>.
                           .<x2>.
                           .<x3>.
                           .<x4>.
                           .<x5>.
                           .<x6>.
                           .<x7>.
                           .<x8>.)) >.
  ;

  Format.fprintf Format.std_formatter "let ps9 = %a@\n"
    Print_code.print_code
    .< fun x1 x2 x3 x4 x5 x6 x7 x8 x9 -> 
      .~(Printf_partially_static.(sprintf
                          ((lit "(" ++ str ++ lit ")")
                           ++
                           (lit "(" ++ str ++ lit ")")
                           ++
                           (lit "(" ++ str ++ lit ")")
                           ++
                           (lit "(" ++ str ++ lit ")")
                           ++
                           (lit "(" ++ str ++ lit ")")
                           ++
                           (lit "(" ++ str ++ lit ")")
                           ++
                           (lit "(" ++ str ++ lit ")")
                           ++
                           (lit "(" ++ str ++ lit ")")
                           ++
                           (lit "(" ++ str ++ lit ")"))
                           .<x1>.
                           .<x2>.
                           .<x3>.
                           .<x4>.
                           .<x5>.
                           .<x6>.
                           .<x7>.
                           .<x8>.
                           .<x9>.)) >.
  ;

end
