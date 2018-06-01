ocamlbuild  -use-ocamlfind -use-menhir -menhir "menhir --infer" -pkgs llvm,llvm.analysis,llvm.bitwriter main.native && mv main.native ./silk
