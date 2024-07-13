open OUnit2
open Living

module Living_ctypes_tests = struct

  let strchr = Ctypes.(Foreign.foreign "strchr" (ptr char @-> char @-> returning (ptr char)))

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
        if !correct = 1000 then assert_failure "Everything passed!"
        )

  let test_liveness_simple =
    (* Define a "safe" strchr *)
    let strchr xs c = Living_core.(strchr xs c => xs) in

    let open Living_core.Let_syntax in
    let open Living_ctypes in
    "Test should pass with Living" >::
      (fun _ -> 
        let correct = ref 0 in
        for _i = 0 to 999 do
          let _ = 
            let* p = CArray.start (CArray.of_string "abc") in
            let* q =  strchr p 'a' in
            let () = Gc.compact () in
            let* c = !@ q in
            if Char.(equal c 'a') then correct := !correct + 1;
            Living_core.return ()
          in ()
        done;
        assert_equal ~msg:"At least one failure" !correct 1000
        )

  let suite = "Living_ctypes tests" >:::
  [ test_liveness_simple;
    test_deadness_simple ]

end

let suite = Living_ctypes_tests.suite

let () = run_test_tt_main suite