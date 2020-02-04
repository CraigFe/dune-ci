(lang dune 2.0)
(name dune-ci)
(implicit_transitive_deps false)

(generate_opam_files true)
(source (github CraigFe/dune-ci))
(maintainers "Craig Ferguson <me@craigfe.io>")
(authors "Craig Ferguson <me@craigfe.io>")

(package
 (name dune-ci)
 (synopsis "CI for the Dune build system")
 (description "\
CI for the Dune build system
")
 (documentation https://CraigFe.github.io/dune-ci/)
 (depends alcotest fmt logs))
