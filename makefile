EXE = smoketest primetime

all:
	dune build $(EXE)
	cp ./_build/default/smoketest/main.exe ./smoketest
	cp ./_build/default/primetime/main.exe ./primetime

run-smoketest:
	dune exec smoketest

run-primetime:
	dune exec primetime

clean:
	dune clean
