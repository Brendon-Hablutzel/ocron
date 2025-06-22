(lang dune 3.19)

(name ocron)

(generate_opam_files true)

(source
 (github Brendon-Hablutzel/ocron))

(authors "Brendon Hablutzel <booklvrbren@gmail.com>")

(license LICENSE)

; (documentation https://url/to/documentation)

(package
 (name ocron)
 (synopsis "Feature-rich, OCaml-based cron alternative")
 (description "An user-friendly alternative to cron, built with OCaml")
 (depends ocaml cmdliner yojson ppx_deriving_yojson)
 (tags
  ("cron" "cli")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
