type dep = Dep : 'a -> dep

type 'a t = { unsafe_value: 'a; dependencies : dep list; name: string option; mutable freed: bool}

let error_msg ?name () =
  Printf.sprintf
    {|Living_core.t value (with name %s) is being garbage collected without 
first being freed with Living_core.unsafe_free.  
This can cause undefined behaviour, so the collection 
of this object has been supressed.  If you really wanted it collected, 
call Living_core.unsafe_free on it first.  Otherwise, this message will display
and you will leak the memory of this value and all its dependencies.

Hint: You can use Living_core.named_return to attempt to label this value for debugging
purposes.|}
    (name |> Option.value ~default:"<unnamed Living_core.t>")
    
let _global = ref { unsafe_value = (); dependencies = []; name = Some "_global"; freed = false }

let construct ?name x deps =
  let ret = {unsafe_value = x; dependencies = deps; name; freed = false} in
  Gc.finalise (fun x ->
    if not x.freed then
      prerr_endline (error_msg ?name:x.name ());
      _global := { !_global with dependencies = Dep x :: !_global.dependencies })
    ret;
  ret

let unsafe_free x =
  x.freed <- true;
  x.unsafe_value

let bind : ('a -> 'b t) -> 'a t -> 'b t =
  fun f x ->
    let y = f (unsafe_free x) in
    let z = { y with dependencies = Dep y :: x.dependencies @ y.dependencies} in
    ignore (unsafe_free y);
    z

let return : 'a -> 'a t =
  fun x -> construct x [Dep x]

let named_return : string -> 'a -> 'a t =
  fun name x -> construct ~name x [Dep x]

let map : ('a -> 'b) -> 'a t -> 'b t =
  fun f x -> { x with unsafe_value = f x.unsafe_value }

let (=>) x y = construct x [Dep x; Dep y]

let (==>) x ys = construct x (Dep x :: List.map (fun y -> Dep y) ys)

let (>>=) x f = bind f x

let keep_alive x = ignore (Sys.opaque_identity x)

module Let_syntax = struct
  let (let*) x f = x |> bind f

  let (let+) x f = x |> map f

  let (let$) x f =
    let ret = f x in
    keep_alive x;
    ret

end
