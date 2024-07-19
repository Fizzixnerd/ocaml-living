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

  let lifetime = Types.Lifetime.static
end

module Make (Config: LIVING_CONFIG) : LIVING_CORE = struct
  open Types
  include Config
  let lifetime = Config.lifetime

  type dep = Dep.t
  type 'a t = 'a Life.t
  let _global = ref { Life.unsafe_value = (); dependencies = Dep.Dep (); name = Some "_global"; lifetime = Lifetime.Static; freed = false }

  let construct ?name x deps =
    let ret = {Life.unsafe_value = x; dependencies = deps; name; lifetime = Config.lifetime; freed = false} in
    Gc.finalise (fun x ->
      if not x.Life.freed then
        if Config.should_log then Config.log_leak x.name;
        if Config.should_prevent_leaks || x.lifetime = Lifetime.static then _global := { !_global with dependencies = Dep (x, !_global.dependencies) })
      ret;
    ret

  let return : 'a -> 'a t =
    fun x -> construct x (Dep x)

  let named_return : string -> 'a -> 'a t =
    fun name x -> construct ~name x (Dep x)

  let unsafe_free x =
    x.Life.freed <- true;
    x.unsafe_value

  let extract = unsafe_free

  let (=>) x y = 
    let z = construct x (Dep (x, y)) in
    let z' = {z with lifetime = y.Life.lifetime}
    in
    ignore (unsafe_free z);
    z'

  let bind : ('a -> 'b t) -> 'a t -> 'b t =
    fun f x ->
      let y = f (unsafe_free x) in
      if y.lifetime <= x.lifetime then
        let z = { y with dependencies = Dep (y, x.dependencies, y.dependencies)} in
        ignore (unsafe_free y);
        z
      else
        raise (Lifetime.Lifetime_exception (y.lifetime, x.lifetime))

  let extend : ('b t -> 'a) -> 'b t -> 'a t =
    fun f x ->
      f x => x

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

module AtMost (Config: LIVING_CONFIG) : LIVING_CORE = struct
  include Make(Config)
  let lifetime = Types.Lifetime.at_most lifetime
end

module Default = Make (Default_living_config)