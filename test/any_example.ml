open Algebra

(** This example shows

    - how to combine different partially-static structures (monoids &
      commutative monoids)

    - that the cotuple operation is useful for partially-static data.
      (It's not really surprising that we need a destructor!  But this
       has been missed in previous work, and it's useful to see that
       the cotuple operation fits nicely.)  -
*)

(** Commutative monoid ⟨∧, ⊤⟩ ... *)
module Band : CMONOID with type t = bool =
struct type t = bool
       let (<+>) x y = x && y
       let zero = true
 end
(** ... and the dynamic version ... *)
module Band_code : CMONOID with type t = bool code =
struct type t = bool code
       let (<+>) x y = .< .~x && .~y >.
       let zero = .< true >.
end
(** ... and the partially-static version *)
module Ps_band =
  Commutative_monoids.PS_commutative_monoid(Band)(Band_code)

(** Commutative monoid ⟨∨, ⊥⟩ ... *)
module Bor : CMONOID with type t = bool =
struct type t = bool
       let (<+>) x y = x || y
       let zero = false
end
(** ... and the dynamic version ... *)
module Bor_code : CMONOID with type t = bool code =
struct type t = bool code
       let (<+>) x y = .< .~x || .~y >.
       let zero = .< false >.
end
(** ... and the partially-static version *)
module Ps_bor =
  Commutative_monoids.PS_commutative_monoid(Bor)(Bor_code)


(** Functor that forgets commutativity.  (This belongs elsewhere) *)
module Monoid_of_cmonoid(C:CMONOID) (* : MONOID with type t = C.t *) =
struct 
  module T = struct type t = C.t end
  module Op = struct
    let (<*>) = C.(<+>)
    let unit = C.zero
  end
end

(** Partially-static monoid for 'int list' *)
module IntListMonoid = Monoids.Free_monoid(struct type t = int end)
module IntListCodeMonoid =
struct
  type t = IntListMonoid.t code
  let unit = .< [] >.
  let (<*>) x y = .< .~x @ .~y >.
end
module Ps_intlist = Monoids.PS_monoid(IntListMonoid)(IntListCodeMonoid)
    
(** The cotuple module that eliminates to partially-static ⟨∧, ⊤⟩ *)
module C1 = Ps_intlist.Eva(Monoid_of_cmonoid(
    struct
      type t = Ps_band.T.t
      include Ps_band.Op
    end))

(** Partially-static for-all function: both argument and result are
    partially static.  The function computes statically and dynamically with the
    respective portions of the input.

    However, note that this currently does too much work, since && isn't lazy.
    Perhaps we can use explicit laziness to make it more efficient.
*)
let for_all_ps : (int -> bool) * (int -> bool) code -> Ps_intlist.T.t -> Ps_band.T.t
  = fun (p, p') ->
    C1.eva
      (fun l -> Ps_band.sta (List.for_all p l))
      (fun l -> Ps_band.var (Aux.var .< List.for_all .~p' .~(Aux.cd_of_var l) >.))
                       
(** The cotuple module that eliminates to partially-static ⟨∨, ⊥⟩ *)
module C2 = Ps_intlist.Eva(Monoid_of_cmonoid(
    struct
      type t = Ps_bor.T.t
      include Ps_bor.Op
    end))

(** Partially-static exists function: both argument and result are
    partially static.  The function computes statically and
    dynamically with the respective portions of the input.

    However, note that this currently does too much work, since || isn't lazy.
    Perhaps we can use explicit laziness to make it more efficient.
*)
let exists_ps : (int -> bool) * (int -> bool) code -> Ps_intlist.T.t -> Ps_bor.T.t
  = fun (p, p') ->
    C2.eva
      (fun l -> Ps_bor.sta (List.exists p l))
      (fun l -> Ps_bor.var (Aux.var .< List.exists .~p' .~(Aux.cd_of_var l) >.))
