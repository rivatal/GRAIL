clear
ocamllex scanner.mll
ocamlyacc parser.mly
ocamlc -c  ast.ml
ocamlc -c astutils.ml
ocamlc -c parser.mli
ocamlc -c scanner.ml
ocamlc -c parser.ml
awk -f imode.awk > igrail.ml
awk -f idebug.awk $1 > infer2
mv infer.ml backupinfer.ml
mv infer2 infer.ml
ocamlc -c infer.ml
ocamlc -c igrail.ml
ocamlc -o grail parser.cmo scanner.cmo astutils.cmo infer.cmo igrail.cmo
rm igrail.ml
mv backupinfer.ml infer.ml
