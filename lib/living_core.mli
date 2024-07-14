(** A [dep] is a dependency of a value of type ['a t].*)
type dep = Dep : 'a -> dep

(** An ['a t] is an value of type ['a] along with its
    dependencies.  In particular, the dependencies cannot be garbage collected while this structure lives.  This
    makes it suitable for encoding dependencies between, say, FFI
    structures and pointers for the garbage collector. *)
type 'a t

(** [bind f x] returns the result of applying the function [f] to
    [x.unsafe_value], adding to it the dependencies of [x] itself. *)
val bind : ('a -> 'b t) -> 'a t -> 'b t

(** Inject an ['a] into an ['a t], whose only dependency is itself.*)
val return : 'a -> 'a t

(** [map f x] maps over the inner [x.unsafe_value] without modifying
    its dependencies.  Note that since [bind], [return], and [(=>)] are 
    the only safe ways of constructing an ['a t], that these dependencies
    always include the never-mapped-over original [unsafe_value]. *)
val map : ('a -> 'b) -> 'a t -> 'b t

(** [x => y] ensures that [y] lives at least as long as [x] does, by wrapping
    [x] in a ['a t] and adding both [x] and [y] as dependencies. *)
val (=>): 'a -> 'b -> 'a t

(** [x ==> ys] ensures that the list of [ys] lives at least as long as [x] does, by
    wrapping [x] in a ['a t] and adding [x] and the list of [ys] as dependencies. *)
val (==>) : 'a -> 'b list -> 'a t

(** See [bind]. *)
val (>>=) : ('a -> 'b t) -> 'a t -> 'b t

(** [keep_alive x] provides a weaker guarantee than ['a t] in that [x] (and not 
    things [x] depends on, if [x] is not of type ['a t]) is kept alive up the
    point where this function is called. *)
val keep_alive : 'a -> unit

module type LET_SYNTAX = sig
  val (let*) : 'a t -> ('a -> 'b t) -> 'b t

  val (let+) : 'a t -> ('a -> 'b) -> 'b t

  (** This is a simple binding operator for keeping the bound variable alive for
      the whole time it is in scope.*)
  val (let$) : 'a -> ('a -> 'b) -> 'b

end

module Let_syntax : LET_SYNTAX