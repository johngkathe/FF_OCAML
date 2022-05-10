(lang dune 2.0)
(using menhir 2.0)
(using dune_site 0.1)
(cram enable)
(name fortissimo)

(authors "John G. Kathe")
(maintainers "johng.kathe@gmail.com")

(generate_opam_files true)

(package
 (name fortissimo)
 (version 0.1)
 (description "A gradually typed web procedural web language")
 (depends
  (menhir (>= 20211230)))
 (sites (frontend)))