open Algebra
open Aux

module S = Make(Type_ops)
include S

module Coproduct_set (A: Algebra) (B: Algebra) =
struct
  module A = A
  module B = B
  module T = struct
    module T = struct type t = Inl of A.T.t | Inr of B.T.t end
    module Op = struct end
  end
  module Eva (C: Algebra) =
  struct
    let eva f g = function
      | T.T.Inl x -> f x
      | T.T.Inr y -> g y
  end
  let inl x = T.T.Inl x
  let inr y = T.T.Inr y
end

module Sets : PRES = struct
  module Free (X : Setoid) =
  struct
    module Alg = struct
      module T = struct type t = X.t end
      module Op = struct end
    end
    let var x = x
  end

  module Bind(X : Setoid) (C : Algebra) =
  struct
    type tx = Free(X).Alg.T.t
    let (>>=) xs f = f xs
  end
  
end

module Split(M : TYPE) = struct module T = M module Op = M end

module PS_set(A : TYPE)(C : TYPE with type t = A.t code) =
  PS(Free_ext_from_coprod(Sets)(Split(A))
       (functor (X : Setoid) ->
        struct
          module Z = Sets.Free(X)
          include Coproduct_set(Split(A))(Z.Alg)
          module Ops = Algebra.Type_ops
        end))
    (Split(C))
