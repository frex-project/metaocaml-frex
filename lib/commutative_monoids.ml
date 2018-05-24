open Algebra
open Aux
open Monads

module Aux'(A : CMONOID)
= struct
  include A

  let (<*>) n a =
    match n with
      n when n < 0 -> raise (Invalid_argument "passed a negative integer")
    | 0 -> A.zero
    | n -> let rec sum n acc =
           match n with
           | 1 -> acc
           | n -> sum (n - 1) (acc <+> a)
      in sum n a
end

module S = Aux.Make(Cmonoid_ops)
include S

module Split(M : CMONOID) = struct module T = M module Op = M end
module Join(M : Algebra) = struct include M.T include M.Op end

module Commutative_monoids =
struct
  module Free(X : Setoid) =
  struct
    module XBag = Bag(X)

    module Alg = struct
      module T = struct type t = XBag.t end
      module Op = struct
        let zero = XBag.zero
        let (<+>) x y = XBag.(<+>) x y
      end
    end

    let var =  XBag.single
  end

  module Bind(X : Setoid)
             (A : Algebra) =
  struct
    let (>>=) xs f =
      let module XBag = Bag(X) in
      let module Aux''  = Aux'(Join(A)) in
      XBag.fold
        (fun x n ys -> Aux''.(ys <+> (n <*> (f x))))
        Aux''.zero
        xs
  end
end

(** Coproduct of commutative monoids *)
module Coproduct_cmonoid (A : Algebra) (B : Algebra) =
struct
  module A = A
  module B = B
  module T = struct
    module T = struct type t = A.T.t * B.T.t end
    module Op = struct
      let zero = (A.Op.zero, B.Op.zero)
      let (<+>) (a, b) (a', b') = (A.Op.(a <+> a'), B.Op.(b <+> b'))
    end
  end
  let inl a = (a, B.Op.zero)
  let inr b = (A.Op.zero, b)
  module Eva (C : Algebra) = struct
    let eva f g (a, b) = C.Op.(f a <+> g b)
  end
end

module PS_commutative_monoid(A : CMONOID)(C : CMONOID with type t = A.t code) =
  PS(Free_ext_from_coprod(Commutative_monoids)(Split(A))
       (functor (X : Setoid) ->
        struct
          module Z = Commutative_monoids.Free(X)
          include Coproduct_cmonoid(Split(A))(Z.Alg)
          module Ops = Algebra.Cmonoid_ops
        end))
    (Split(C))
