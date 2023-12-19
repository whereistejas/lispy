(lang dune 3.12)

(name lispy)

(generate_opam_files true)

(source
 (github whereistejas/lispy))

(authors "Tejas Sanap")

(package
 (name lispy)
 (synopsis "A toy Lisp implementation")
 (depends
  ocaml
  dune
  (ppx_inline_test
   (>= 0.16.1))
  (ppx_expect
   (>= 0.16.0))))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
