open Aux

(** Does the following module type and functor result in inefficient
code? (I.e., will OCaml inline the definition of cd? **)

module type PRES =  (* Just a (relative) monad! *)
sig
  module Free ( X : Setoid )
    : sig
      type t
      val var : X.t -> t
    end

  module Bind( X : Setoid )
             ( Y : Setoid )
    : sig
      type tx = Free(X).t
      type ty = Free(Y).t

      val (>>=) : tx -> (X.t -> ty) -> ty
    end
end

module type ALGEBRA =
sig
  module Pres : PRES
  type t

  open Pres

  module Bind( X : Setoid )
    : sig
      type free = Free(X).t

      val (>>=) : free -> (X.t -> t) -> t
    end
end


module Free (Pres : PRES) (X : Setoid)
  : ALGEBRA with module Pres = Pres
             and type   t = Pres.Free(X).t
= struct
  module Pres = Pres

  open Pres

  type t = Free(X).t

  module Bind( Y : Setoid )
  = struct
    type free = Free(Y).t

    module YXBind = Bind(Y)(X)

    let (>>=) = YXBind.(>>=)
  end
end

module type DEFER_DATA =
sig
  module A : ALGEBRA

  module Lift( X : Setoid ) :
  sig
    type free = A.Pres.Free(X).t

    val lift : free -> (X.t -> A.t code)
           -> A.t code
  end
end

module Defer_algebra(Defer : DEFER_DATA)
  : ALGEBRA with module Pres = Defer.A.Pres
             and type   t    = Defer.A.t code
= struct
  module Pres = Defer.A.Pres
  type   t    = Defer.A.t code

  module Bind( X : Setoid )
  = struct
    type free = Pres.Free(X).t

    module XLift = Defer.Lift(X)

    let (>>=) = XLift.lift
  end
end


module type FREE_EXT =
sig
  module Algebra : ALGEBRA

  open Algebra.Pres

  type sta = Algebra.t

  module PS : functor (X : Setoid) ->
    sig
      include  ALGEBRA with module Pres = Algebra.Pres


      val  sta :  sta -> t
      val  var :  X.t -> t

      module Eva ( C : ALGEBRA with module Pres = Pres) :
      sig
        val  eva : (sta -> C.t) -> (X.t -> C.t) -> t -> C.t
      end
    end
end


module PS_from_FREE_EXT (Free_ext : FREE_EXT)
                        (Defer    : DEFER_DATA with module A = Free_ext.Algebra)
  = struct
   module V = Var(Free_ext.Algebra)

   module Code = Defer_algebra(Defer)
   module PS  = Free_ext.PS(V)

   include PS


   type sta = Free_ext.sta
   let sta  = PS.sta
   let dyn x= PS.var (Aux.var x)

   module EvaCode = PS.Eva (Code)
   let cd   = EvaCode.eva
       (fun x -> .< x >.)
       Aux.cd_of_var
end
