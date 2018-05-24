module S : Aux.Sig with module Ops := Algebra.Type_ops

module Coproduct_set (A : S.Algebra) (B : S.Algebra) :
  S.COPRODUCT with module A.T = A.T with module B.T = B.T

module PS_set (A : Algebra.TYPE) (C : Algebra.TYPE with type t = A.t code) :
  S.PS with type A.T.t = A.t

