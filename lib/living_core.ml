type dep = Dep : 'a -> dep

type 'a t = { unsafe_value: 'a; dependencies : dep list}

let bind : ('a -> 'b t) -> 'a t -> 'b t =
  fun f x ->
    let y = f x.unsafe_value in
    { y with dependencies = Dep y :: x.dependencies @ y.dependencies}

let return : 'a -> 'a t =
  fun x -> { unsafe_value = x; dependencies = [Dep x]}

let map : ('a -> 'b) -> 'a t -> 'b t =
  fun f x -> { x with unsafe_value = f x.unsafe_value }

let (=>) x y = { unsafe_value = x; dependencies = [Dep x; Dep y]}

let (==>) x ys = {unsafe_value = x; dependencies = Dep x :: List.map (fun y -> Dep y) ys}

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
