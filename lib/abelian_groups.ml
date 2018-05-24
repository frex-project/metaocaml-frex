open Algebra
open Aux

module Aux'(A : CMONOID) =
struct
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

module S = Make(Cgroup_ops)
include S

module Split(M : CGROUP) = struct module T = M module Op = M end
module Join(M : Algebra) = struct include M.T include M.Op end

module Abelian_groups : PRES =
struct
  module Free(X : Setoid) =
  struct
    module Alg = struct
      module Op = Bag(X)
      module T = Op
    end
    let var =  Alg.Op.single
  end

  module Bind(X : Setoid) (A : Algebra) =
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

module Coproduct_cgroup (A : Algebra) (B : Algebra) =
struct
  module Y = Commutative_monoids.Coproduct_cmonoid(A)(B)
  module T = struct
    module T = Y.T.T
    module Op = struct
      include Y.T.Op
      let inv (a, b) = (A.Op.inv a, B.Op.inv b)
    end
  end
  module A = A
  module B = B
  let inl, inr = Y.(inl, inr)
  module Eva (C : Algebra) = struct
    let eva = let module Z = Y.Eva (C) in Z.eva
  end
end

module PS_commutative_group(A : CGROUP)(C : CGROUP with type t = A.t code) =
  PS(Free_ext_from_coprod(Abelian_groups)(Split(A))
       (functor (X : Setoid) ->
        struct
          module Z = Abelian_groups.Free(X)
          include Coproduct_cgroup(Split(A))(Z.Alg)
          module Ops = Algebra.Cgroup_ops
        end))
    (Split(C))
