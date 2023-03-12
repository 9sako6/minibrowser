(lang dune 3.6)

(name minibrowser)

(generate_opam_files true)

(source
 (github 9sako6/minibrowser))

(authors "9sako6")

(maintainers "9sako6")

(license MIT)

(documentation https://url/to/documentation)

(package
 (name minibrowser)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends
  ocaml
  dune
  base
  core
  lablgtk3
  ppx_deriving
  (ppx_expect :with-test)
  (ppx_inline_test :with-test))
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
