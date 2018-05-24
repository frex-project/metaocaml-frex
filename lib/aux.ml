open Algebra

module type Setoid = sig
  include TYPE
  val compare : t -> t -> int
end

(** Some setoids *)
type void = {elim_empty : 'a. 'a}
module Zero  = struct
  type t = void
  let compare _ _ = 0
  let whatever {elim_empty} = elim_empty
end

module One = struct
  type t = unit
  let compare _ _ = 0
end

module Two  = struct
  type t = bool
  let compare x y = match x,y with
                      false, false ->  0
                    | false, true  -> -1
                    | true , false ->  1
                    | true , true  ->  0
end

module type PARTIALLY_STATIC =
sig
  (* These names are different from those currently used in the paper,
     but the interface is otherwise the same. *)
  type t
  type sta
  val sta : sta -> t
  val dyn : sta code -> t
  val cd : t -> sta code
end

type 'a var = Var of 'a code * int

let var_counter = ref 0

let var : 'a. 'a code -> 'a var =
  fun x -> incr var_counter; Var (x, !var_counter)

let cd_of_var : 'a. 'a var -> 'a code =
  fun (Var (c, _)) -> c

(** Intended to represent variables, but can represent any code.
    Isomorphic to A.t code, but also comparable. *)
module Var (A: sig type t end) : Setoid with type t = A.t var =
struct
  type t = A.t var
  let compare (Var (_, l)) (Var (_, r)) = Pervasives.compare l r
end

(** Bags *)

let mergefn (+) k l r =
  match l, r with
    None, None -> None
  | Some v, None
  | None, Some v -> Some v
  | Some l, Some r ->
    Some (l + r)

module Bag (A: Setoid) :
sig
  include CMONOID with type t = int Map.Make(A).t

  val (<*>) : int -> t -> t
  val single : A.t -> t
  val inv : t -> t
  val fold : (A.t -> int -> 'a -> 'a) -> 'a -> t -> 'a

  val compare : t -> t -> int
end =
struct
  module M = Map.Make(A)
  type t = int M.t
  let single v = M.singleton v 1
  let zero = M.empty
  let (<+>) = M.merge (mergefn (+))
  let fold f empty t = M.fold f t empty
  let inv = M.map (fun n -> -n)

  let (<*>) n xs = M.map (fun k -> n * k) xs

  let compare = M.compare Pervasives.compare
end

module Int =
struct
  type t = int
  let compare = Pervasives.compare

  let (<+>) = ( + )
  let (<*>) = ( * )

  let neg x = -x

  let zero = 0
  let unit = 1
end

module Code (X : Setoid) : TYPE with type t = X.t code =
struct
  type t = X.t code
end

module Int_code =
struct
  type t = int code
  let (<+>) x y = .< .~x + .~y >.
  let (<*>) x y = .< .~x * .~y >.
  let zero = .< 0 >.
  let unit = .< 1 >.
  let neg x = .< - .~x >.
end


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

  module type Free_extension = sig
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
  module type COPRODUCT = sig
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


module Make (Ops : functor (X: TYPE) -> sig module type OP end) =
struct
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

  module type Free_extension = sig
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
  module type COPRODUCT = sig
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
    : Free_extension with module A.T = A.T =
  struct
    module A = A

    module PS(X : Setoid) = struct
      module CX = C(X)
      module Alg = CX.T

      let sta = CX.inl
      let var x = let module FX = Pres.Free(X) in CX.inr (FX.var x)

      module Eva (D : Algebra) = struct
        let eva h e c =
          let module Eva = CX.Eva(D) in
          let module Bind= Pres.Bind(X)(D) in
          Eva.eva h Bind.(fun xs -> xs >>= e) c
      end
    end
  end

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
           (Defer : Algebra with type T.t = Ext.A.T.t code) = struct
    module A = Ext.A
    include Ext.PS(Var(Ext.A.T))
    include Alg
    module CodeEva = Eva(Defer)
    let cd = CodeEva.eva (fun a -> .< a >.) cd_of_var
  end
end
