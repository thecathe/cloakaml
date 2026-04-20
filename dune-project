(lang dune 3.22)

(name cloakaml)

(generate_opam_files true)

(source
 (github thecathe/cloakaml))

(authors "Jonah Pears <author@example.com>")

(maintainers "Jonah Pears <maintainer@example.com>")

(license BSD-3-Clause)

; (documentation https://url/to/documentation)

(package
 (name cloakaml)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml deriving odoc yojson)
 (tags
  ("add topics" "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
