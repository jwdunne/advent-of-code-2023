(lang dune 3.12)

(name aoc2023)

(generate_opam_files true)

(source
 (github jwdunne/advent-of-code-2023))

(authors "James Dunne")

(license MIT)

(package
 (name aoc2023)
 (synopsis "My attempt at learning OCaml doing AoC")
 (depends ocaml dune core)
 (tags
  (advent-of-code learning)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
