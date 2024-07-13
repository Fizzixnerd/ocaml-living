(lang dune 3.15)

(name living)

(generate_opam_files true)

(version 0.1.0)

(source
 (github Fizzixnerd/ocaml-living))

(authors "Matt Walker")

(maintainers "Matt Walker")

(license LICENSE)

(package
 (name living)
 (synopsis "A safer FFI interface")
 (description "An interface to Ctypes' ffi that lives harmoniously with the OCaml garbage collector.")
 (depends ocaml dune ctypes)
 (tags
  (ffi ctypes ounit2)))

