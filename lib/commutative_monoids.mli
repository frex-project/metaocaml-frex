module S : Aux.Sig with module Ops := Algebra.Cmonoid_ops

module Coproduct_cmonoid (A : S.Algebra) (B : S.Algebra) :
  S.COPRODUCT with module A.T = A.T and module B.T = B.T
              with type T.T.t = A.T.t * B.T.t

module PS_commutative_monoid
    (A : Algebra.CMONOID) (C : Algebra.CMONOID with type t = A.t code)
  : S.PS with type A.T.t = A.t
