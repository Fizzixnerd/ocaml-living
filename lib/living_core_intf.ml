module Types = struct

  module Dep = struct
    type t = Dep : 'a -> t
  end

  module Lifetime = struct
    type id = int
    type t = Static | AtMost of id * t

    let static = Static

    let partial_compare x y =
      let rec occurs id lifetime =
        match lifetime with
        | Static -> false
        | AtMost (lifetime_id, lifetime') -> lifetime_id = id || occurs id lifetime'
      in
      match (x, y) with
      | (Static, Static) -> Some 0
      | (_, Static) -> Some (-1)
      | (Static, _) -> Some 1
      | (AtMost (idx, _), AtMost (idy, _)) -> 
        if idx = idy then Some 0
        else if occurs idx y then Some (-1)
        else if occurs idy x then Some 1
        else None

    let next_id : id ref = ref 0
    let get_next_id : unit -> id = 
      fun () -> 
        let next = !next_id in
        let () = next_id := !next_id + 1 in
        next

    let at_most l = AtMost (get_next_id(), l)

    let (<=) x y = 
      match partial_compare x y with
      | Some (-1) | Some 1 -> true
      | _ -> false

    exception Lifetime_exception of t * t
  end
  module Life = struct
    type 'a t = { unsafe_value: 'a; dependencies : Dep.t; name: string option; lifetime: Lifetime.t; mutable freed: bool}
  end
end

module type LIVING_TYPES = sig
  module Dep : sig
    type t
  end
  module Lifetime: sig
    type id
    type t

    (** The "static" lifetime: the longest lifetime. *)
    val static : t

    (** Implements a partial ordering on [t]*)
    val partial_compare : t -> t -> int option

    (** Construct a lifetime that is at most as long as the argument. *)
    val at_most : t -> t

    val (<=) : t -> t -> bool

    exception Lifetime_exception of t * t
  end
  module Life : sig
    type 'a t 
  end
end

(* module type LIVING_TYPES = sig
(** A [dep] is a dependency of a value of type ['a t].*)
  type dep = Dep : 'a -> dep

  (** An ['a t] is an value of type ['a] along with its
      dependencies.  In particular, the dependencies cannot be garbage 
      encoding dependencies between, say, FFI structures and pointers 
      collected while this structure lives.  This makes it suitable for
      for the garbage collector. *)
  type 'a t
end
 *)
module type LIVING_CONFIG = sig
  val log_leak : string option -> unit
  val should_log: bool
  val should_prevent_leaks: bool

  val lifetime: Types.Lifetime.t
end

module type LIVING_CORE = sig
  include LIVING_CONFIG

  type dep = Types.Dep.t
  type 'a t = 'a Types.Life.t

  (** [bind f x] returns the result of applying the function [f] to
      [x.unsafe_value], adding to it the dependencies of [x] itself. *)
  val bind : ('a -> 'b t) -> 'a t -> 'b t

  val extend : ('b t -> 'a) -> 'b t -> 'a t

  (** [return x] injects an ['a] into an ['a t], whose only dependency is itself.*)
  val return : 'a -> 'a t

  (** [named_return name x] injects an [x : 'a] into an ['a t], whose only dependency
      is itself.  Provides an explicit [name : string] for help with debugging
      erroneous garbage collections without a call to [unsafe_free].*)
  val named_return : string -> 'a -> 'a t

  (** [unsafe_free x] allows [x] to be garbage collected, and prevents the normal warning
      that accompanies a ['a Living_core.t] being garbage collected.  Beware: this
      is an unsafe operation that can lead to segfaults and bad data.  It is
      recommended that you either bundle all structs and pointers that comprise the
      ['a]'s true dependencies into another type (in the case that you need to 
      maintain access to foreign data), or marshall all the data you want to OCaml
      values directly (when you don't).
      
      Consider the use of this function an optimization that you should measure your
      need for before you reach for it. *)
  val unsafe_free : 'a t -> 'a

  (** UNSAFE FUNCTION: same as [unsafe_free]. *)
  val extract : 'a t -> 'a

  (** [map f x] maps over the inner [x.unsafe_value] without modifying
      its dependencies.  Note that since [bind], [return], and [(=>)] are 
      the only safe ways of constructing an ['a t], that these dependencies
      always include the never-mapped-over original [unsafe_value]. *)
  val map : ('a -> 'b) -> 'a t -> 'b t

  (** [x => y] ensures that [y] lives at least as long as [x] does, by wrapping
      [x] in a ['a t] and adding both [x] and [y] as dependencies.  Note that this
      operator works on OCaml lists, tuples, and arrays too, if you need to keep
      multiple objects as dependencies. *)
  val (=>): 'a -> 'b t -> 'a t

  (** See [bind]. *)
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t

  (** [keep_alive x] provides a weaker guarantee than ['a t] in that [x] (and not 
      things [x] depends on, if [x] is not of type ['a t]) is kept alive up the
      point where this function is called. *)
  val keep_alive : 'a -> unit

  module Let_syntax : sig
    val (let*) : 'a t -> ('a -> 'b t) -> 'b t
  
    val (let+) : 'a t -> ('a -> 'b) -> 'b t
  
    (** This is a simple binding operator for keeping the bound variable alive for
        the whole time it is in scope.*)
    val (let$) : 'a -> ('a -> 'b) -> 'b
  end
end

