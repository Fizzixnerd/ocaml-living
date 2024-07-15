# Living

This is an addon library for `Ctypes` that provides stronger guarantees about the lifetimes of foreign object allocated from OCaml.  See [this blog post](https://fizzixnerd.com/blog/2024-07-11-a-possibly-safer-interface-to-the-ctypes-ffi/) for the original motivation.

## Classic Example of the Problem `Living` Solves

Consider the following plain `Ctypes` code:

```ocaml
open Ctypes

(** Returns a pointer into the argument character string that points to the first
    instance of the argument character. *)
let strchr : char ptr -> char -> char ptr = 
  Foreign.foreign "strchr" (ptr char @-> char @-> returning (ptr char))

let () =
  let p = CArray.of_string "abc" |> CArray.start in
  let q = strchr p 'a' in
  let () = Gc.compact () in
  let c = !@ q in
  if Char.(equal c 'a') then print_endline "yay!" else print_endline "boo!"
```

This code will almost always print "boo!" (run `dune test` if you don't believe me!).  The issue is that `p` does not appear after the `Gc.compact ()`, and since the garbage collector has no knowledge of the implicit dependency between `p`, `q`, and `c`, in that `q` points into `p` (and so its dereference `c` is only valid as long as `p` continues to live) it collects `p` during compaction.

The idea of this library is to provide a way of encoding this dependency semi-automatically.

The first step is to define a `strchr` that knows about the dependency of its return value on its argument.

```ocaml
open Living
open Living_ctypes

let strchr  : char ptr -> char -> char ptr Living_core.t = 
  let strchr_unsafe = Foreign.foreign "strchr" (ptr char @-> char @-> returning (ptr char)) in
  fun s c -> Living_core.(strchr_unsafe s c => s)
```

Here, we have used `(=>)` operator to encode the information that `strchr_unsafe s c` being alive _implies_ that `s` must be alive too, in order for the program to be correct.  Next, we replace any operations that can create dependant values in `Ctypes` with their `Living` counterparts, and replace the `let`s that bind them with `let*`s instead.  We also need to return a value of type `'a Living.t`, so we just return a `unit` wrapped in this type.

```ocaml
let _ =
  let open Living_core.Let_syntax in
  let* p = CArray.of_string "abc" |> Living_core.bind CArray.start in
  let* q = strchr p 'a' in
  let () = Gc.compact () in
  let* c = !@ q in
  if Char.(equal c 'a') then print_endline "yay!" else print_endline "boo!"
  Living_core.return ()
```

Other than that, the code is now correct.  Run it and it will _always_ print "yay!".  That's all there is to it!

## Wrapping C Functions Properly

The key step we needed to do manually is to encode the dependence of `strchr_unsafe`'s return value on its argument.  This can take many forms, as C has many ways of returning values.  The details are up to the FFI binding author to get right, but here are some hints:

1. Always add to pointers into structures their dependence on the structure.
2. Rewrite "output"-pointer-containing functions to return tuples instead, so that dependency is easier to track.

## Dropping Dependencies

It is often useful to allow the GC to collect garbage, so you generally don't want your whole program to be wrapped in a `Living_core.t` containing every dependency in it.  Remember however, that this is an _optimization_, and should only be attempted once you know you need to by measuring performance.  If you mess this up, you can get segfaults, and often it's good enough to just let stuff fall out of scope.

If you have measured performance and found you need to drop dependencies, you can do so by calling the `Living_core.unsafe_free` function.  This returns the current value of the computation _without_ its dependencies.  Some care must be taken however.

The process looks like this:

1. Take your `'a Living_core.t` and figure out if it has any pointers or structures that have been allocated by `malloc`, `Ctypes.allocate`, `Ctypes.allocate_n` or the like.
2. If it does not, proceed to step 4.
3. If it does, then copy all that data into OCaml heap objects that can't be GC'd from underneath you like an off-heap pointer can be, by using `Living_core.map` or `Living_core.bind` to map the `'a` to a new, safer `'b`
4. Call `Living_core.unsafe_free`.

It is important to do step 3. properly.  Here are two examples; the first you should never do.

```ocaml
(* Bad Example *)
(*** NEVER DO THIS ***)
let _ =
  let my_dependencyless_char =
    CArray.of_string "abc"
    |> Living_core.bind CArray.start
    |> Living_core.bind (fun q -> strchr p 'a') in
    (* Bad assumption: We don't care about q after we derefence it, since the char is copied to OCaml, so we use the non-wrapped version of !@ from base Ctypes on just the value. *)
    |> fun my_dependencyful_char_ptr -> Ctypes.(!@) (Living_core.unsafe_free my_dependencyful_char_ptr)
  in
  Printf.printf "%c\n" my_dependencyless_char
```

The problem is that, at least theoretically, `Ctypes.(!@)` could call the garbage collector _before_ it dereferences the pointer `q.unsafe_value`.  This would land us back in hot water.  Instead, prefer the following idiom:

```ocaml
(* Good Example *)
(*** DO THIS ***)
let _ =
  let my_dependencyless_char =
    CArray.of_string "abc"
    |> Living_core.bind CArray.start
    |> Living_core.bind (fun p -> strchr p 'a')
    |> Living_core.map Ctypes.(!@) (* Key idea: map with !@ _inside_ the Living_core.t context! *)
    |> Living_core.unsafe_free (* And only the call unsafe_free *)
  in
  Printf.printf "%c\n" my_dependencyless_char
```

That is, you should do all the mapping you need to do to get to a safe, OCaml-copied value _within_ the context of the `Living_core.t`, before finally calling `Living_core.unsafe_free`.

In this second example, even if `Ctypes.(!@)` calls the garbage collector, the `Living` library ensures that the C string "abc" will not be collected out from under you.

## Configuring the Library

`Living_core` can be configured in a variety of ways using the `Living_core.Make` functor.

1. One may provide their own logging function `log_leak`, which is passed an `string option` possibly containing the name of the leaked `Living_core.t`
2. One may disable leak logging entirely by setting `should_log` to `false`.
3. One may disable the safety net of preventing leaking of `Living_core.t`s which haven't been `unsafe_free`d by setting `should_prevent_leaks` to false.  This is an optimization and only recommended if you're sure you've got stuff right.  Segfaults await the uncareful programmer.

One my use default implementations of all of these things by accessing the `Living_core.Living_config_default` or even more simply by using the instantiated functor `Living_core.Default`.

## Using the Library

If you choose to use `Living` library in a project `Foo` then please _also_ make it a functor of type `module Living_core_intf.LIVING_CORE -> module FOO`.  This way users of your module can configure the `LIVING_CORE` implementation used to agree with other libraries they are using.  Some users might prefer to disable logging, to log to some special logger, or to disable safety after optimizing their usage, for example.  However, I would imagine many bindings would prefer to not expose their explicit dependence on `Living` at _all_, so if this is the case feel free to configure the module yourself -- just know you might be limiting some specific class of users.  An alternative is to provide a default implementation, as `Living_ctypes` and `Living_core` do.