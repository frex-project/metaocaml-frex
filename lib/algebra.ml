(** Sets *)
module type TYPE = sig
  type t
end

module Type_ops (T: TYPE) =
struct
  module type OP = sig end
end

(** Commutative monoids *)
module type CMONOID =
sig
  type t
  val zero : t
  val (<+>) : t -> t -> t
end

module Cmonoid_ops (T: TYPE) =
struct
  module type OP = CMONOID with type t := T.t
end

(** Monoids *)
module type MONOID = sig
  type t
  val unit : t
  val (<*>) : t -> t -> t
end

module Monoid_ops (T: TYPE) =
struct
  module type OP = MONOID with type t := T.t
end

(** Commutative rings *)
module type CRING =
sig
  include CMONOID
  include MONOID with type t := t
  val neg : t -> t
end

module Cring_ops (T: TYPE) =
struct
  module type OP = CRING with type t := T.t
end

(** Abelian groups *)
module type CGROUP =
sig
  include CMONOID
  val inv : t -> t
end

module Cgroup_ops (T: TYPE) =
struct
  module type OP = CGROUP with type t := T.t
end
