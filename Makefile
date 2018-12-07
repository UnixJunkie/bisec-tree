.PHONY: test install uninstall reinstall doc clean

all:
	dune build @install

test:
	dune build test.exe
	_build/default/test.exe

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
