(** A [dep] is a dependency of a value of type ['a t].*)
type dep = Dep : 'a -> dep

(** An ['a t] is an [unsafe_value: 'a] along with its
    [dependencies: dep list].  In particular, the dependencies
    cannot be garbage collected while this structure lives.  This
    makes it suitable for encoding dependencies between, say, FFI
    structures and pointers for the garbage collector. *)
type 'a t = { unsafe_value: 'a; dependencies : dep list}

(** [bind f x] returns the result of applying the function [f] to
    [x.unsafe_value], adding to it the dependencies of [x] itself. *)
let bind : ('a -> 'b t) -> 'a t -> 'b t =
  fun f x ->
    let y = f x.unsafe_value in
    { y with dependencies = Dep y :: x.dependencies @ y.dependencies}

(** Inject an ['a] into an ['a t], whose only dependency is itself.*)
let return : 'a -> 'a t =
  fun x -> { unsafe_value = x; dependencies = [Dep x]}

(** [map f x] maps over the inner [x.unsafe_value] without modifying
    its dependencies.  Note that since [bind], [return], and [(=>)] are 
    the only safe ways of constructing an ['a t], that these dependencies
    always include the never-mapped-over original [unsafe_value]. *)
let map : ('a -> 'b) -> 'a t -> 'b t =
  fun f x -> { x with unsafe_value = f x.unsafe_value }

(** [x => y] ensures that [y] lives at least as long as [x] does, by wrapping
    [x] in a ['a t] and adding both [x] and [y] as dependencies.*)
let (=>) x y = { unsafe_value = x; dependencies = [Dep x; Dep y]}

(** See [bind]. *)
let (>>=) x f = bind f x

module Let_syntax = struct
  let (let*) x f = x |> bind f

  let (let+) x f = x |> map f
end
