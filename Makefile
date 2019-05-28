.PHONY: test simplify install uninstall reinstall doc clean

all:
	dune build @install

test:
	dune build src/test.exe
	_build/default/src/test.exe

simplify:
	dune build src/simplify.exe
	_build/default/src/simplify.exe -k 50 data/bunny.txt \
	  > data/bunny_simple.txt
	wc -l data/bunny.txt data/bunny_simple.txt

install: all
	dune install

uninstall:
	dune uninstall

reinstall: install reinstall

doc:
	mkdir -p doc
	ocamldoc -html -d doc bisec_tree.mli

clean:
	rm -rf _build
