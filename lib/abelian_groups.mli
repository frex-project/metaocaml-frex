module S : Aux.Sig with module Ops := Algebra.Cgroup_ops

module Coproduct_cgroup (A : S.Algebra) (B : S.Algebra) :
  S.COPRODUCT with module A.T = A.T and module B.T = B.T

module PS_commutative_group (A : Algebra.CGROUP)
    (C : Algebra.CGROUP with type t = A.t code) :
  S.PS with type A.T.t = A.t
