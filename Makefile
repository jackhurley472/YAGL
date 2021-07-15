# "make test" Compiles everything and runs the regression tests

.PHONY : test
test : all testall.sh
	./testall.sh

# "make all" builds the executable as well as the "stdlib" library designed
# to link external code

.PHONY : all
all : yagl.native stdlib.o

# "make yagl.native" compiles the compiler
#
# The _tags file controls the operation of ocamlbuild, e.g., by including
# packages, enabling warnings
#
# See https://github.com/ocaml/ocamlbuild/blob/master/manual/manual.adoc

yagl.native :
	opam config exec -- \
	ocamlbuild -use-ocamlfind yagl.native

# "make clean" removes all generated files

.PHONY : clean
clean :
	ocamlbuild -clean
	rm -rf testall.log ocamlllvm *.diff
	rm -rf stdlib.o
	rm -rf fail-*.err
	rm -rf test-*.out
	rm -rf test-*.ll
	rm -rf test-*.s
	rm -rf test-*.diff
	rm -rf test-*.exe

# Testing the "stdlib" for our built-in functions

stdlib : stdlib.c
	cc -o stdlib -DBUILD_TEST stdlib.c

