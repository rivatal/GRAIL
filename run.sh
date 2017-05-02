clear
ocamllex scanner.mll
ocamlyacc parser.mly
ocamlc -c  ast.ml
ocamlc -c astutils.ml
ocamlc -c parser.mli
ocamlc -c scanner.ml
ocamlc -c parser.ml
ocamlc -c infer.ml
awk -f imode.awk > igrail.ml
ocamlc -c igrail.ml
ocamlc -o grail parser.cmo scanner.cmo astutils.cmo infer.cmo igrail.cmo
rm igrail.ml
