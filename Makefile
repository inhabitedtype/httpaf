.PHONY: all build clean test examples

build:
	dune build @install

all: build

test:
	dune runtest

examples:
	dune build @examples

watch:
	dune build {httpaf,httpaf-async,httpaf-lwt-unix}.install @runtest --watch

install:
	dune install

uninstall:
	dune uninstall

clean:
	rm -rf _build *.install
