include Living_core_intf
module Default_living_config : LIVING_CONFIG = struct
  let log_leak name =
    Printf.sprintf
      {|Living_core.t value (with name %s) is being garbage collected without 
  first being freed with Living_core.unsafe_free.  
  This can cause undefined behaviour, so the collection 
  of this object has been supressed.  If you really wanted it collected, 
  call Living_core.unsafe_free on it first.  Otherwise, this message will display
  and you will leak the memory of this value and all its dependencies.  To configure 
  this behavior, see Living_core.LIVING_CONFIG and Living_core.Make.
  
  Hint: You can use Living_core.named_return to attempt to label this value for debugging
  purposes.|}
    (name |> Option.value ~default:"<unnamed Living_core.t>")
    |> prerr_endline
  let should_log = true
  let should_prevent_leaks = true
end

module Types = struct
  type dep = Dep : 'a -> dep

  type 'a t = { unsafe_value: 'a; mutable dependencies : dep; name: string option; mutable freed: bool}
end

module Make (Config: LIVING_CONFIG) : LIVING_CORE = struct
  open Types

  type nonrec dep = dep
  type nonrec 'a t = 'a t
  let _global = ref { unsafe_value = (); dependencies = Dep (); name = Some "_global"; freed = false }

  let construct ?name x deps =
    let ret = {unsafe_value = x; dependencies = deps; name; freed = false} in
    Gc.finalise (fun x ->
      if not x.freed then
        if Config.should_log then Config.log_leak x.name;
        if Config.should_prevent_leaks then !_global.dependencies <- Dep (x, !_global.dependencies))
      ret;
    ret

  let return : 'a -> 'a t =
    fun x -> construct x (Dep x)

  let named_return : string -> 'a -> 'a t =
    fun name x -> construct ~name x (Dep x)

  let (=>) x y = construct x (Dep (x, y))

  let add_dep : 'a t -> 'b -> unit =
    fun x y ->
      x.dependencies <- Dep (x.dependencies, y)

  let unsafe_free x =
    x.freed <- true;
    x.unsafe_value

  let bind : ('a -> 'b t) -> 'a t -> 'b t =
    fun f x ->
      let y = f (unsafe_free x) in
      y.dependencies <- Dep (y, x.dependencies, y.dependencies);
      y

  let (>>=) x f = bind f x

  let map : ('a -> 'b) -> 'a t -> 'b t =
    fun f x -> { x with unsafe_value = f x.unsafe_value }

  let keep_alive x = ignore (Sys.opaque_identity x)

  module Let_syntax = struct
    let (let*) x f = x |> bind f

    let (let+) x f = x |> map f

    let (let$) x f =
      let ret = f x in
      keep_alive x;
      ret
  end
end


module Default = Make (Default_living_config)