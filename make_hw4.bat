call make_hw1.bat
call make_hw3.bat

ocamlc -o hw2_inference hw1.cmo hw2_unify.cmo hw2_inference.mli hw2_inference.ml
