open Algebra
open Aux
open Monads

(** In sufficiently complicated examples, maybe it's best to first
    implement the free extension? **)

(* auxiliary operations for commutative rings *)
module CR_aux(A : CRING)
=
struct
  include A

  let is_odd n = n mod 2 = 1

  let rec (<^>) a n =
    match n with
      0 -> A.unit
    | 1 -> a
    | n when is_odd n -> A.(let x = (a <^> (n-1/2)) in (x <*> x) <*> a)
    | n (*otherwise*) -> A.(let x = (a <^> (n  /2)) in x <*> x)


  let two = A.(unit <+> unit)

  (* ℕ is the initial commutative ring *)
  let rec init n =
    match n with
      0 -> A.zero
    | 1 -> A.unit
    | n when n < 0 -> raise (Invalid_argument "passed a negative integer")
    | n when is_odd n -> A.((two <*> (init ((n - 1)/2))) <+> unit)
end

module S = Aux.Make(Cring_ops)

include S

(** Multinomials with coefficients in A *)
module Multinomial (A: CRING) (X: Setoid)
  : sig
    include CRING with type t = A.t Map.Make(Bag(X)).t
    val var : X.t -> t
    val scalar : A.t -> t

    module Eva (B : CRING) :
    sig
      val eva : (A.t -> B.t) -> (X.t -> B.t) -> t -> B.t
    end
  end =
struct
  (** a₀ + a₁{x₁₁ᵉ¹ … xₙ₁ᵉⁿ} + a₂{x₁₂bᵉ¹ … xₙ₂ᵉⁿ} + … *)
  module BX = Bag(X)
  module M = Map.Make(Bag(X))

  type t = A.t Map.Make(Bag(X)).t

  let zero = M.empty
  let unit = M.singleton BX.zero A.unit
  let (<+>) = M.merge (mergefn A.(<+>))
  let neg : t -> t = M.map A.neg

  let mul1 a x m =
    (* aⱼ{x₁ⱼᵉ¹ … xₙⱼᵉⁿ}
       (b₀ + b₁{x₁₁ᵉ¹ … xₙ₁ᵉⁿ} + b₂{x₁₂ᵉ¹ … xₙ₂ᵉⁿ} + … + bₘ{x₁ₘᵉ¹ … xₙₘᵉⁿ}) *)
    M.fold (fun x' b sum -> M.singleton BX.(x <+> x') A.(a <*> b) <+> sum) m M.empty

  (** Not as in the paper, for the moment; this just distributes. *)
  let (<*>) p q = M.fold (fun x a sum -> mul1 a x q <+> sum) p M.empty


  (*"scalar" multiplication *)
  let (<.>) a xs = M.map (fun a' -> A.(a <*> a')) xs

  let var    x = M.singleton  BX.(single x) A.unit
  let scalar a = M.singleton (BX.zero    ) a

  (* Substitution *)
  module Eva (B : CRING) =
  struct
    let eva h e xss =
      let module BAux = CR_aux(B) in
      M.fold
        (fun xs a sum ->
           BX.fold
             (fun x n prod -> BAux.(prod <*> ((e x) <^> n)))
             (h a)
             xs
        )
        xss
        B.zero
  end
end

module Split(M : CRING) = struct module T = M module Op = M end
module Join(M : Algebra) = struct include M.T include M.Op end

module Commutative_rings =
struct
  module Free (X : Setoid) =
  struct
    module Alg = struct
      module Op = Multinomial (Int)(X)
      module T = Op
    end
    let var = Alg.Op.var
  end

  module Bind (X : Setoid) (A : Algebra) =
  struct
    let (>>=) xss f =
      let module XMult= Multinomial(Int)(X) in
      let module Bind = XMult.Eva(Join(A)) in
      let module Y = CR_aux(Join(A)) in
      Bind.eva Y.init f xss
  end
end

(** Multinomials with coefficients in ℕ *)
module Free_commutative_ring (X: Setoid) = Multinomial (Int) (X)

module Coproduct_cring_free_cring
    (A: sig module T : TYPE module Op : Cring_ops(T).OP end)
    (X: Setoid) :
  COPRODUCT with module A.T = A.T
             and module B.T = Free_commutative_ring(X) =
struct
  module T = struct
    module Op = Multinomial(Join(A))(X)
    module T = struct type t = Multinomial(Join(A))(X).t end
  end
  module BX = Bag(X)
  module M = Map.Make(Bag(X))

  let is_odd n = n mod 2 = 1

  let twoA = A.Op.(unit <+> unit)

  (* The initial homomorphism from ℕ to A
     See CR_aux.init above
  *)
  let rec a_of_n = function
    | 0 -> A.Op.zero
    | 1 -> A.Op.unit
    | n when is_odd n -> A.Op.(unit <+> a_of_n (pred n))
    | n -> A.Op.(twoA <*> a_of_n (n / 2))

  let inl a = M.singleton BX.zero a
  let inr x = M.map a_of_n x (* Is this the right definition? *)

  module Eva (C : Algebra) = struct
    let eva f g m =
      (M.fold
         (fun b a c -> (* Is this the right definition? *)
            C.Op.((f a <*> g (M.singleton b 1)) <+> c))
         m
         C.Op.zero)
  end
  module A = A
  module B = struct
    module T = Free_commutative_ring(X)
    module Bops' = Split(T)
    module Op = Bops'.Op
  end
end

module PS_csr(A : CRING) (C: CRING with type t = A.t code) =
  PS(Free_ext_from_coprod(Commutative_rings)(Split(A))
       (functor (X : Setoid) ->
        struct
          include Coproduct_cring_free_cring(Split(A))(X)
          module Ops = Algebra.Cring_ops
        end))
    (Split(C))
