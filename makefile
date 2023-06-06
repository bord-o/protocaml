EXE = smoketest primetime

all:
	dune build $(EXE)

run-smoketest:
	dune exec smoketest

run-primetime:
	dune exec primetime

clean:
	dune clean
