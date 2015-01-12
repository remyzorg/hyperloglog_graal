



all:
	ocamlbuild main.native

clean:
	ocamlbuild -clean
	@rm -f test


test:
	g++ -Wall -O2 $@.cc -o $@

run-test: test
	./test ./main.native
