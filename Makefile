.PHONY: fmt test

test:
	dune runtest

fmt:
	dune build @fmt --auto-promote
