EXE = smoketest primetime

all: clean
	
	dune build $(EXE)
	cp ./_build/default/smoketest/main.exe ./smoketest
	cp ./_build/default/primetime/main.exe ./primetime
	chmod +x ./smoketest/main.exe
	chmod +x ./primetime/main.exe

run-smoketest:
	dune exec smoketest

run-primetime:
	dune exec primetime

clean:
	rm -f ./smoketest/main.exe
	rm -f ./primetime/main.exe
	dune clean
