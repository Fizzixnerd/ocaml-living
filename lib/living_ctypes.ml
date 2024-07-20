module Make (LCore: Living_core_intf.LIVING_CORE) = struct

    include Ctypes

    (** [!@ p] dereferences the pointer [p], wrapped in an [LCore.t].
        The dependencies include the original pointer.  If the reference
        type is a scalar type then dereferencing constructs a new value.
        If the reference type is an aggregate type then dereferencing
        returns a value that references the memory pointed to by p. *)
    let (!@) p = LCore.(!@ p => p)

    (** If [p] is a pointer to an array element then [p +@ n] computes
        the address of the [n]th next element, wrapped in an [LCore.t].
        The dependencies include the original pointer. *)
    let (+@) p n = LCore.(p +@ n => p)

    (** If [p] is a pointer to an array element then [p +@ n] computes
        the address of the [n]th previous element, wrapped in an [LCore.t].
        The dependencies include the original pointer. *)
    let (-@) p n = LCore.(p -@ n => p)

    (** [allocate t v] allocates a fresh value of type [t], initialises it
        with [v] and returns its address, wrapped in an [LCore.t].  
        The dependencies include the argument [v]. The argument [?finalise],
        if present, will be called just before the memory is freed. The value
        will be automatically freed after no references to the pointer remain
        within the calling OCaml program. *)
    let allocate ?finalise typ x = LCore.(allocate ?finalise typ x => x)

    (** [allocate_n ~count t] allocates [count] fresh values of type [t], and
        returns its address, wrapped in an [LCore.t].  The dependencies include
        only the returned value, but the value is returned within an [LCore.t] so
        that dependencies can be tracked when you later use [setf] or [(<-@)] with
        it. 
        
        The argument [?finalise], if present, will be called just before the 
        memory is freed. The value will be automatically freed after no references
        to the pointer remain within the calling OCaml program.*)
    let allocate_n ?finalise ~count typ = LCore.(return (allocate_n ?finalise ~count typ))

    (** [getf s f] retrieves the value of the field [f] in the structure or
        union [s], wrapped in an [LCore.t].  The dependencies include the 
        original structure.  The semantics for non-scalar types are
        non-copying, as for [(!@)]. *)
    let getf s f = LCore.(getf s f => s)

    (** [setf s f v] overwrites the value of the field [f] in the structure or union
        [s] with [v], and returns a [unit] wrapped in an [LCore.t].  The dependencies
        include [v]. *)
    let setf s f x = 
        LCore.add_dep s x;
        LCore.map (fun s' -> setf s' f x) s

    (** [s @. f] computes the address of the field [f] in the structure or
        union value [s], wrapped in an [LCore.t].  The dependencies include
        the original structure. *)
    let (@.) s f = LCore.(s @. f => s)

    (** [p |-> f] computes the address of the field [f] in the structure or
        union value pointed to by [p], wrapped in an [LCore.t].  The
        dependencies include the original pointer. *)
    let (|->) p f = LCore.(p |-> f => p)

    (** [p <-@ v] writes the value [v] to the address [p], and returns a [unit]
        wrapped in an [LCore.t].  The dependencies include [v]. *)
    let (<-@) p x = 
        LCore.add_dep p x;
        LCore.map (fun p' -> p' <-@ x) p

    (** [addr s] returns the address of the structure or union [s], wrapped
        in a [LCore.t].  The dependencies include the original structure. *)
    let addr s = LCore.(addr s => s)

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
        let get a n = LCore.(get a n => a)

        (** [set a n v] overwrites the [n]th element of the zero-indexed array [a] with [v].

            If you rebind the [CArray] module to [Array] then you can also use the [a.(n) <- v]
            syntax instead of [Array.set a n v].

            Raise [Invalid_argument "index out of bounds"] if [n] is outside of the range [0]
            to [(CArray.length a - 1)]. *)
        let set a n x = 
            LCore.add_dep a x;
            LCore.map (fun a' -> set a' n x) a

        (** [map t f a] is analogous to [Array.map f a]: it creates a new array with
            element type [t] whose elements are obtained by applying [f] to the
            elements of [a], except the result is wrapped in an [LCore.t].  The
            dependencies include the original array. *)
        let map t f a = LCore.(=>) (map t f a) a

        (** [mapi] behaves like {!Array.mapi}, except that it also passes the
            index of each element as the first argument to [f] and the element
            itself as the second argument.  The result is wrapped in an [LCore.t].
            The dependencies include the original array. *)  
        let mapi t f a = LCore.(mapi t f a => a)

        (** [CArray.fold_left (@) x a] computes 
                [(((x @ a.(0)) @ a.(1)) ...) @ a.(n-1)]
            where [n] is the length of the array [a].  The result is wrapped in a
            [LCore.t].  The dependencies include the original array. *)
        let fold_left f x a = LCore.(fold_left f x a => a)

        (** [CArray.fold_right f a x] computes
                [a.(0) @ (a.(1) @ ( ... (a.(n-1) @ x) ...))]
            where [n] is the length of the array [a].  The result is wrapped in a
            [LCore.t].  The dependencies include the original array. *)
        let fold_right f a x = LCore.(fold_right f a x => a)

        (** Return the address of the first element of the given array, wrapped in a
            [LCore.t].  The dependencies include the original array. *)
        let start a = LCore.(start a => a)

        (** [from_ptr p n] creates an [n]-length array reference to the memory at
            address [p], wrapped in an [LCore.t].  The dependencies include
            the original pointer. *)
        let from_ptr p n = LCore.(from_ptr p n => p)

    end
end

module Default = Make(Living_core.Default)