(lang dune 2.9)

(name tcaml)

(version 0.0.1)

(generate_opam_files true)

(source
 (github Totsugekitai/tcaml))

(license MIT)

(authors "Totsugekitai<37617413+Totsugekitai@users.noreply.github.com>")

(maintainers "Totsugekitai<37617413+Totsugekitai@users.noreply.github.com>")

(package
 (name tcaml)
 (synopsis "toy ocaml compiler")
 (description "tiny toy ocaml compiler")
 (depends
  (ounit2 :with-test)))

(using menhir 2.1)
