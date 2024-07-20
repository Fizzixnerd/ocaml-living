open OUnit2
open Living
module Living_core = Living_core.Default
module Living_ctypes = Living_ctypes.Default

module Living_ctypes_tests = struct

  let strchr = Ctypes.(Foreign.foreign "strchr" (ptr char @-> char @-> returning (ptr char)))

  type t
  let s : t Ctypes.structure Ctypes.typ = Ctypes.structure "dummy"
  let x_f = Ctypes.field s "x" (Ctypes.ptr Ctypes.int)
  let () = Ctypes.seal s

  let test_deadness_simple =
    let open Ctypes in
    "Test should usually fail because of UB" >::
      (fun _ ->
        let correct = ref 0 in
        for _i = 0 to 999 do
          let p = CArray.start (CArray.of_string "abc") in
          let q = strchr p 'a' in
          let () = Gc.compact () in
          let c = !@ q in
          if Char.(equal c 'a') then correct := !correct + 1
        done;
        if !correct = 1000 then assert_failure "q was fine!")

  let test_liveness_simple =
    (* Define a "safe" strchr *)
    let strchr xs c = Living_core.(strchr xs c => xs) in

    let open Living_core.Let_syntax in
    let open Living_ctypes in
    "Test should pass with Living" >::
      (fun _ ->
        let correct = ref 0 in
        for _i = 0 to 999 do
          let x = 
            let* p = CArray.start (CArray.of_string "abc") in
            let* q =  strchr p 'a' in
            let () = Gc.compact () in
            let* c = !@ q in
            if Char.(equal c 'a') then correct := !correct + 1;
            Living_core.named_return "final value" ()
          in Living_core.unsafe_free x
        done;
        assert_equal ~cmp:Int.equal ~msg:"At least one failure" !correct 1000)
        
  let test_deadness_set =
    let open Ctypes in
    "Test should usually fail because of UB" >::
    (fun _ ->
      let correct = ref 0 in
      for _i = 0 to 999 do
        let y = allocate_n ~count:1 s in
        let x = allocate int 7 in
        let x' = y |-> x_f in
        let () = x' <-@ x in
        let () = Gc.compact () in
        let x'' = !@ !@ x' in
        if x'' = 7 then correct := !correct + 1
      done;
      if !correct = 1000 then assert_failure "x' didn't die!")

  let test_liveness_set =
    let open Living_core.Let_syntax in
    let open Living_ctypes in
    "Test pass with Living" >::
    (fun _ ->
      let correct = ref 0 in
      for _i = 0 to 999 do
        let x = 
          let* y = allocate_n ~count:1 s in
          let* x = allocate int 7 in
          let x' = y |-> x_f in
          let* () = x' <-@ x in
          let () = Gc.compact () in
          let* x'' = Living_core.bind (!@) (Living_core.bind (!@) x') in
          if x'' = 7 then correct := !correct + 1;
          Living_core.named_return "final value" ()
        in Living_core.unsafe_free x
      done;
      assert_equal ~cmp:Int.equal ~msg:"At least one failure" !correct 1000) 

  let test_ptr = Living_ctypes.allocate_n ~count:1 s

  let test_no_use_after_free =
    let open Living_core.Let_syntax in
    let open Living_ctypes in
    "Test avoids use-after-free bug present in https://fizzixnerd.com/blog/2024-07-17-touring-the-living-library/" >::
    (fun _ ->
      let correct = ref 0 in
      for _i = 0 to 999 do
        let y = 
          let n = Random.int 100 in
          let* x = allocate int n in
          let* test_ptr' = test_ptr in
          let x' = test_ptr' |-> x_f in
          let* () = x' <-@ x in
          let () = Gc.compact () in
          let* x'' = Living_core.bind (!@) (Living_core.bind (!@) x') in
          if x'' = n then correct := ! correct + 1;
          Living_core.named_return "final_value" ()
      in Living_core.unsafe_free y
    done;
    assert_equal ~cmp:Int.equal ~msg:"At least one failure" !correct 1000)

  let suite = "Living_ctypes tests" >:::
  [ test_liveness_simple;
    test_deadness_simple;
    test_liveness_set;
    test_deadness_set;
    test_no_use_after_free ]

end

let suite = Living_ctypes_tests.suite

let () = run_test_tt_main suite

let () = Gc.compact()