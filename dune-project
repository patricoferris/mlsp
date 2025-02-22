(lang dune 3.17)

(name mdlsp)

(generate_opam_files true)

(source
 (github patricoferris/mdlsp))

(authors "Patrick Ferris <patrick@sirref.org>")

(maintainers "Patrick Ferris <patrick@sirref.org>")

(license MIT)

(package
 (name mdlsp)
 (synopsis "Markdown LSP Toolkit")
 (description "Build custom markdown LSP servers")
 (depends
   ocaml
   cmarkit
   eio
   lsp
 )
 (tags
  (lsp markdown)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
