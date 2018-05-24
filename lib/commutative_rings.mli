module S : Aux.Sig with module Ops := Algebra.Cring_ops

module Free_commutative_ring (X : Aux.Setoid) :
sig
  include Algebra.CRING
  val var : X.t -> t
  val scalar : int -> t
  module Eva (B : Algebra.CRING) :
  sig val eva : (int -> B.t) -> (X.t -> B.t) -> t -> B.t end
end

module Coproduct_cring_free_cring (A : S.Algebra) (X : Aux.Setoid) :
  S.COPRODUCT with module A.T = A.T

module PS_csr (A : Algebra.CRING) (C: Algebra.CRING with type t = A.t code) :
  S.PS with type A.T.t = A.t

