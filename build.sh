ocamlbuild  -use-ocamlfind -use-menhir -pkgs llvm,llvm.analysis,llvm.bitwriter main.native && mv main.native ./silk
