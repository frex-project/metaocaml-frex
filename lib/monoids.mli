module S : Aux.Sig with module Ops := Algebra.Monoid_ops

module Coproduct_monoid (A : S.Algebra) (B : S.Algebra) :
  S.COPRODUCT with module A.T = A.T with module B.T = B.T

module Free_monoid (A : Algebra.TYPE) :
  Algebra.MONOID with type t = A.t list

module PS_monoid (A : Algebra.MONOID)
    (C : Algebra.MONOID with type t = A.t code) : S.PS with type A.T.t = A.t

