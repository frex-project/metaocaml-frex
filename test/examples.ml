(* To compile you will need to install BER MetaOCaml: opam switch 4.04.0+BER *)
open Runcode
open Algebra
open Aux
open Monads


(* I very much doubt the following is an efficient way to go about
   things, because we introduce another level of indirection when
   choosing operations, but it's a generic way to do it.  *)

(** Commutative monoids *)
(** Bags *)

module Commutative_Monoids : PRES with module Free = Bag = struct
  module Free (A : Setoid) = Bag(A)

  module Var (A : Setoid)  = struct
    module BA = Bag(A)
    let var = BA.single
  end

  module Bind (A : Setoid)(B: Setoid) = struct
    module BA = Bag(A)
    module BB = Bag(B)
    let (>>=) a_s f = BB.(
                   BA.fold (fun a a_count result -> (a_count <*> f a) <+> result)
                           zero
                           a_s )
  end
end

module type Commutative_Monoid = ALGEBRA with module Pres = Commutative_Monoids


module CMONOID(C : Commutative_Monoid) : CMONOID with type t = C.t = struct
  type t = C.t
  module BZ = Bag(Zero)
  module CZ = C.Bind(Zero)
  let zero = CZ.( BZ.zero >>= Zero.whatever )

  module VTwo = C.Pres.Var(Two)
  module BTwo = Bag(Two)
  module CTwo = C.Bind(Two)
  let two = VTwo.(BTwo.(<+>) (var true) (var false ))
  let (<+>) a b = CTwo.( two >>= fun c -> if c then a else b )
end

(* An example commutative semiring *)
module Boolean_csemiring : CSEMIRING with type t = bool =
struct
  type t = bool
  let zero = false
  let unit = true
  let (<+>) = (||)
  let (<*>) = (&&)
end
