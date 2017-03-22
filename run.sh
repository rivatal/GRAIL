clear
ocamllex scanner.mll
ocamlyacc parser.mly
ocamlc -c ast.mli
ocamlc -c astutils.ml
ocamlc -c parser.mli
ocamlc -c scanner.ml
ocamlc -c parser.ml
ocamlc -c infer.ml
ocamlc -c grail.ml
ocamlc -o grail parser.cmo scanner.cmo astutils.cmo infer.cmo grail.cmo

