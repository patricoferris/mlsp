(lang dune 3.17)

(name mlsp)

(generate_opam_files true)

(source
 (github patricoferris/mlsp))

(authors "Patrick Ferris <patrick@sirref.org>")

(maintainers "Patrick Ferris <patrick@sirref.org>")

(license MIT)

(package
 (name mlsp)
 (synopsis "LSP Toolkit")
 (description "Build custom LSP servers")
 (depends
   ocaml
   eio
   lsp
 )
 (tags
  (lsp)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
