all: check-compiler ps.cma ps.cmxa

tests=printf_benchmark dot_product printf_example any_example # examples

OCAMLBUILD=ocamlbuild -use-ocamlfind -ocamlc '-toolchain metaocaml ocamlc' \
                                     -ocamlopt '-toolchain metaocaml ocamlopt'

test: $(foreach test,$(tests),$(test).byte)
	for test in $(tests); do ./$$test.byte; done

install:
	ocamlfind install frex META \
          _build/lib/ps.cma         \
          _build/lib/*.cmi          \
          _build/lib/*.mli

uninstall:
	$(OCAMLBUILD) remove frex

clean:
	$(OCAMLBUILD) -clean

%.cma %.cmxa %.native %.byte:
	$(OCAMLBUILD) -use-ocamlfind $@

check-compiler:
	@test $$(opam switch  show) = "4.07.1+BER"  \
      || test $$(opam switch  show) = "4.04.0+BER"  \
      || (echo 1>&2 "Please use OPAM switch 4.04.0+BER or 4.07.1+BER"; exit 1)

.PHONY: check-compiler all clean test
