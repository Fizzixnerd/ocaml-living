include Ctypes

(** [!@ p] dereferences the pointer [p], wrapped in a [Living.t].  The dependencies include the original pointer.  If the reference type is a scalar type then dereferencing constructs a new value. If the reference type is an aggregate type then dereferencing returns a value that references the memory pointed to by p. *)
let (!@) p = Living.(!@ p => p)

(** If [p] is a pointer to an array element then [p +@ n] computes the address of the [n]th next element, wrapped in a [Living.t].  The dependencies include the original pointer. *)
let (+@) p n = Living.(p +@ n => p)

(** If [p] is a pointer to an array element then [p +@ n] computes the address of the [n]th previous element, wrapped in a [Living.t].  The dependencies include the original pointer. *)
let (-@) p n = Living.(p -@ n => p)

(* Should ocaml_string/byte_start be here? *)

(** [getf s f] retrieves the value (wrapped in a [Living.t]) of the field [f] in the structure or union [s].  The dependencies include the original structure.  The semantics for non-scalar types are non-copying, as for [(!@)]. *)
let getf s f = Living.(getf s f => s)

let (@.) s f = Living.(s @. f => s)

let (|->) p f = Living.(p |-> f => p)

let addr s = Living.(addr s => s)

module CArray = struct
  include Ctypes.CArray

  let get a n = Living.(get a n => a)

  let map t f a = Living.(=>) (map t f a) a

  let mapi t f a = Living.(mapi t f a => a)

  let fold_left f x a = Living.(fold_left f x a => a)

  let fold_right f a x = Living.(fold_right f a x => a)

  let start a = Living.(start a => a)

  let from_ptr p n = Living.(from_ptr p n => p)

end