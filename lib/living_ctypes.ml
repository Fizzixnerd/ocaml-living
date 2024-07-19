module Make (LCore: Living_core_intf.LIVING_CORE) = struct

    include Ctypes

    (** [!@ p] dereferences the pointer [p], wrapped in a [LCore.t].
        The dependencies include the original pointer.  If the reference
        type is a scalar type then dereferencing constructs a new value.
        If the reference type is an aggregate type then dereferencing
        returns a value that references the memory pointed to by p. *)
    let (!@) p = LCore.map (!@) p

    (** If [p] is a pointer to an array element then [p +@ n] computes
        the address of the [n]th next element, wrapped in a [LCore.t].
        The dependencies include the original pointer. *)
    let (+@) p n = LCore.map (fun p' -> p' +@ n) p

    (** If [p] is a pointer to an array element then [p +@ n] computes
        the address of the [n]th previous element, wrapped in a [LCore.t].
        The dependencies include the original pointer. *)
    let (-@) p n = LCore.map (fun p' -> p' -@ n) p

    (** [allocate t v] allocates a fresh value of type [t], initialises it
        with [v] and returns its address, wrapped in a [LCore.t].  
        The dependencies include the argument [v]. The argument [?finalise],
        if present, will be called just before the memory is freed. The value
        will be automatically freed after no references to the pointer remain
        within the calling OCaml program. *)
    let allocate ?finalise typ x = LCore.map (fun x' -> allocate ?finalise typ x') x

    (** [getf s f] retrieves the value of the field [f] in the structure or
        union [s], wrapped in a [LCore.t].  The dependencies include the 
        original structure.  The semantics for non-scalar types are
        non-copying, as for [(!@)]. *)
    let getf s f = LCore.map (fun s -> getf s f) s

    (** [setf s f v] overwrites the value of the field [f] in the structure or union
        [s] with [v], and returns a [unit] wrapped in a [LCore.t].  The dependencies
        include [v]. *)
    let setf s f x = 
        s 
        |> LCore.bind (fun s' -> 
            LCore.map (setf s' f) x)

    (** [s @. f] computes the address of the field [f] in the structure or
        union value [s], wrapped in a [LCore.t].  The dependencies include
        the original structure. *)
    let (@.) s f = LCore.map (fun s' -> s' @. f) s

    (** [p |-> f] computes the address of the field [f] in the structure or
        union value pointed to by [p], wrapped in a [LCore.t].  The
        dependencies include the original pointer. *)
    let (|->) p f = LCore.map (fun p' -> p' |-> f) p

    (** [p <-@ v] writes the value [v] to the address [p], and returns a [unit]
        wrapped in a [LCore.t].  The dependencies include [v]. *)
    let (<-@) p x = p |> LCore.bind (fun p' -> LCore.map (fun x' -> p' <-@ x') x)

    (** [addr s] returns the address of the structure or union [s], wrapped
        in a [LCore.t].  The dependencies include the original structure. *)
    let addr s = LCore.map addr s

    (** Operations on C arrays. *)
    module CArray = struct
        include Ctypes.CArray

        (** [get a n] returns the [n]th element of the zero-indexed array [a], wrapped
            in a [LCore.t].  The dependencies include the original array.  The 
            semantics for non-scalar types are non-copying, as for {!(!@)}.

            If you rebind the [CArray] module to [Array] then you can also use the
            syntax [a.(n)] instead of [Array.get a n].

            Raise [Invalid_argument "index out of bounds"] if [n] is outside of the
            range [0] to [(CArray.length a - 1)]. *)
        let get a n = LCore.map (fun a' -> get a' n ) a

        (** [set a n v] overwrites the [n]th element of the zero-indexed array [a] with [v].

            If you rebind the [CArray] module to [Array] then you can also use the [a.(n) <- v]
            syntax instead of [Array.set a n v].

            Raise [Invalid_argument "index out of bounds"] if [n] is outside of the range [0]
            to [(CArray.length a - 1)]. *)
        let set a n x = a |> LCore.bind (fun a' -> LCore.map (set a' n) x)

        (** [map t f a] is analogous to [Array.map f a]: it creates a new array with
            element type [t] whose elements are obtained by applying [f] to the
            elements of [a], except the result is wrapped in a [LCore.t].  The
            dependencies include the original array. *)
        let map t f a = LCore.map (map t f) a

        (** [mapi] behaves like {!Array.mapi}, except that it also passes the
            index of each element as the first argument to [f] and the element
            itself as the second argument.  The result is wrapped in a [LCore.t].
            The dependencies include the original array. *)  
        let mapi t f a = LCore.map (mapi t f) a 

        (** [CArray.fold_left (@) x a] computes 
                [(((x @ a.(0)) @ a.(1)) ...) @ a.(n-1)]
            where [n] is the length of the array [a].  The result is wrapped in a
            [LCore.t].  The dependencies include the original array. *)
        let fold_left f x a = LCore.map (fold_left f x) a

        (** [CArray.fold_right f a x] computes
                [a.(0) @ (a.(1) @ ( ... (a.(n-1) @ x) ...))]
            where [n] is the length of the array [a].  The result is wrapped in a
            [LCore.t].  The dependencies include the original array. *)
        let fold_right f a x = LCore.map (fun a' -> fold_right f a' x) a

        (** Return the address of the first element of the given array, wrapped in a
            [LCore.t].  The dependencies include the original array. *)
        let start a = LCore.map start a

        (** [from_ptr p n] creates an [n]-length array reference to the memory at
            address [p], wrapped in a [LCore.t].  The dependencies include
            the original pointer. *)
        let from_ptr p n = LCore.map (fun p' -> from_ptr p' n) p

    end
end

module Default = Make(Living_core.Default)