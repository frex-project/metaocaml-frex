open Algebra
open Aux
open Monads

module S = Aux.Make(Monoid_ops)
include S

module Monoids : PRES =
struct
  module Free (X : Setoid) =
  struct
    module Alg = struct
      module T = struct type t = X.t list end
      module Op = struct
        let unit = []
        let (<*>) = (@)
      end
    end

    let var (x : X.t) = [x]
  end

  module Bind(X : Setoid)
             (C : Algebra) =
  struct
    type tx = Free(X).Alg.T.t

    let (>>=) (xs : tx) (f : X.t -> C.T.t) : C.T.t = match xs with
      | [x] -> f x
      | xs -> List.fold_right C.Op.(<*>) (List.map f xs) C.Op.unit
  end
end

module Split(M : MONOID) = struct module T = M module Op = M end

(* Singleton types *)
type a = A and b = B

type ('a, 'b, 'start) alt =
    Empty : ('a, 'b, _) alt
  | ConsA : 'a * ('a, 'b, b) alt -> ('a, 'b, a) alt
  | ConsB : 'b * ('a, 'b, a) alt -> ('a, 'b, b) alt

type ('a, 'b) monoid_coproduct =
  M : ('a, 'b, _) alt -> ('a, 'b) monoid_coproduct

(* Can probably be a lot more efficient if we use the representation
   in Prop 4 instead of the coproduct.

   Actually, perhaps not. We're happy to do lots of computation up
   front if it means that the generated code is efficient.

   The code below has some redudancy (see Prop. 3 in the note, or the
   big comment below): we're never checking that we're cancelling out
   the units.
*)
(** Coproduct of monoids *)
module Coproduct_monoid (A : Algebra) (B : Algebra) =
struct
  module A = A
  module B = B
  type t' = (A.T.t, B.T.t) monoid_coproduct

  let consA : type start. A.T.t -> (A.T.t, B.T.t, start) alt -> (A.T.t, B.T.t, a) alt =
    fun a -> function
      | Empty -> ConsA (a, Empty)
      | ConsA (a', m) -> ConsA (A.Op.(<*>) a a', m)
      | ConsB _ as r -> ConsA (a, r)

  let consB : type start. B.T.t -> (A.T.t, B.T.t, start) alt -> (A.T.t, B.T.t, b) alt =
    fun b -> function
      | Empty -> ConsB (b, Empty)
      | ConsA _ as r -> ConsB (b, r)
      | ConsB (b', m) -> ConsB (B.Op.(<*>) b b', m)

  let rec mul : type start start'.
    (A.T.t, B.T.t, start) alt -> (A.T.t, B.T.t, start') alt -> t' =
    fun l r ->
      (* Note: this does *not* currently eliminate units that result
         from multiplications.  We don't have a way of identifying
         units available at present.

         There is a way to eliminate units from multiplication in the
         coproduct of a monoid with a free monoid (see Prop. 4 in the
         notes), but if we're multiplying a lot of free elements in
         sequence, this more efficient representation will generate
         intermediate units when generating code, a price we might not
         be willing to pay.
      *)
      match l, r with
      | l, Empty -> M l
      | Empty, r -> M r
      | ConsA (a, m), r -> let M m' = mul m r in M (consA a m')
      | ConsB (b, m), r -> let M m' = mul m r in M (consB b m')
  module T = struct
    module T = struct type t = t' end
    module Op = struct
      let (<*>) (M l) (M r) = mul l r
      let unit = M Empty
    end
  end
  let inl a = M (ConsA (a, Empty))
  let inr b = M (ConsB (b, Empty))
  module Eva (C : Algebra) = struct
    let eva f g (M c) =
      let rec cot' : type start.(A.T.t, B.T.t, start) alt -> C.T.t = function
        | Empty -> C.Op.unit
        | ConsA (a, Empty) -> f a (* non-empty list optimization: drop the unit *)
        | ConsB (b, Empty) -> g b (* ditto *)
        | ConsA (a, m) -> C.Op.(<*>) (f a) (cot' m)
        | ConsB (b, m) -> C.Op.(<*>) (g b) (cot' m)
      in cot' c
  end
end

module Free_monoid(A: TYPE) :
  MONOID with type t = A.t list =
struct
  type t = A.t list
  let unit = []
  let (<*>) = (@)
end

module PS_monoid(A : MONOID)(C : MONOID with type t = A.t code) =
  PS(Free_ext_from_coprod(Monoids)(Split(A))
       (functor (X : Setoid) ->
        struct
          module Z = Monoids.Free(X)
          include Coproduct_monoid(Split(A))(Z.Alg)
          module Ops = Algebra.Monoid_ops
        end))
    (Split(C))
