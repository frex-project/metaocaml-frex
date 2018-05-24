open Algebra

(* Note: Setoid = Map.OrderedType *)
module type Setoid = sig
  include TYPE
  val compare : t -> t -> int
end

type _ var
val var : 'a code -> 'a var
val cd_of_var : 'a var -> 'a code

module Var (A: sig type t end) : Setoid with type t = A.t var

module Bag (A: Setoid) :
sig
  include CMONOID with type t = int Map.Make(A).t

  val (<*>) : int -> t -> t
  val single : A.t -> t
  val inv : t -> t
  val fold : (A.t -> int -> 'a -> 'a) -> 'a -> t -> 'a

  val compare : t -> t -> int
end

(* TODO: does this need to be exposed? *)
val mergefn : ('a -> 'a -> 'a) -> _ -> 'a option -> 'a option -> 'a option

module Int :
sig
  type t = int
  include Setoid  with type t := t
  include MONOID  with type t := t
  include CMONOID with type t := t
  val neg : t -> t
end

module Int_code :
sig
  type t = int code
  include MONOID  with type t := t
  include CMONOID with type t := t
  val neg : t -> t
end

type void = {elim_empty : 'a. 'a}

module Zero : sig
  include Setoid with type t = void
  val whatever : void -> 'a
end
module One : Setoid with type t = unit
module Two : Setoid with type t = bool

module type Sig = sig
  module Ops (X: TYPE) : sig module type OP end

  module type Algebra = sig
    module T : TYPE
    module Op : Ops(T).OP
  end

  module type PRES = sig
    module Free(X : Setoid) : sig
      module Alg : Algebra
      val var : X.t -> Alg.T.t
    end

    module Bind(X : Setoid) (C : Algebra) : sig
      val (>>=) : Free(X).Alg.T.t -> (X.t -> C.T.t) -> C.T.t
    end
  end

  module type Free_extension =
  sig
    module A : Algebra

    module PS(X : Setoid) : sig
      module Alg : Algebra
      val var : X.t -> Alg.T.t
      val sta : A.T.t -> Alg.T.t

      module Eva (C : Algebra) : sig
        val eva : (A.T.t -> C.T.t) -> (X.t -> C.T.t) -> Alg.T.t -> C.T.t
      end
    end
  end

  (** Coproducts *)
  module type COPRODUCT =
  sig
    module T : Algebra
    module A : Algebra
    module B : Algebra

    val inl : A.T.t -> T.T.t
    val inr : B.T.t -> T.T.t

    module Eva (C: Algebra) : sig
      val eva : (A.T.t -> C.T.t) -> (B.T.t -> C.T.t) -> T.T.t -> C.T.t
    end
  end

  module Free_ext_from_coprod (Pres : PRES) (A : Algebra)
      (C : functor (X : Setoid) ->
          COPRODUCT with module A.T = A.T
                     and module B.T = Pres.Free(X).Alg.T)
    : Free_extension with module A.T = A.T 

  module type PS = sig
    module A : Algebra
    include Algebra
    val var : A.T.t var -> T.t
    val sta : A.T.t -> T.t
    val cd : T.t -> A.T.t code
    module Eva (C : Algebra) :
    sig val eva : (A.T.t -> C.T.t) -> (A.T.t var -> C.T.t) -> T.t -> C.T.t end
  end

  module PS(Ext   : Free_extension)
           (Defer : Algebra with type T.t = Ext.A.T.t code) :
    PS with module A.T = Ext.A.T
end

module Make (Ops : functor (X: TYPE) -> sig module type OP end) :
  Sig with module Ops := Ops
