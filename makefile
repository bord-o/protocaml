EXE = smoketest primetime

all: clean
	
	dune build $(EXE)
	cp ./_build/default/smoketest/main.exe ./bin/smoketest
	cp ./_build/default/primetime/main.exe ./bin/primetime
	chmod +x ./bin/smoketest
	chmod +x ./bin/primetime

run-smoketest:
	./bin/smoketest

run-primetime:
	./bin/primetime

clean:
	rm -f ./bin/smoketest
	rm -f ./bin/primetime
	dune clean
