.PHONY: test

all:
	jbuilder build @install

test:
	jbuilder build test.exe
	./_build/default/test.exe

install: all
	jbuilder install

uninstall:
	ocamlfind -remove bisec

clean:
	rm -rf _build
