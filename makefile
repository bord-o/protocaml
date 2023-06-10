EXE = smoketest primetime

all: clean
	
	dune build $(EXE)
	cp ./_build/default/smoketest/main.exe ./bin/smoketest
	cp ./_build/default/primetime/main.exe ./bin/primetime
	cp ./_build/default/primetime/main.exe ./bin/means

	chmod +x ./bin/smoketest
	chmod +x ./bin/means
	chmod +x ./bin/primetime

run-smoketest:
	./bin/smoketest

run-primetime:
	./bin/primetime

run-means:
	./bin/means

clean:
	rm -f ./bin/smoketest
	rm -f ./bin/primetime
	rm -f ./bin/means
	dune clean
