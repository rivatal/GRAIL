ocamllex scanner.mll
ocamlyacc parser.mly
<<<<<<< HEAD
ocamlc -c  ast.ml
ocamlc -c astutils.ml
=======
ocamlc -c ast.mli
>>>>>>> 1458cb2d0df975b0b960729576e84ae4f0c061ba
ocamlc -c parser.mli
ocamlc -c scanner.ml
ocamlc -c parser.ml
