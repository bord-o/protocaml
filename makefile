EXE = smoketest primetime means chat

all: clean
	
	dune build $(EXE)
	cp ./_build/default/smoketest/main.exe ./bin/smoketest
	cp ./_build/default/primetime/main.exe ./bin/primetime
	cp ./_build/default/means/main.exe ./bin/means
	cp ./_build/default/chat/main.exe ./bin/chat

	chmod +x ./bin/smoketest
	chmod +x ./bin/means
	chmod +x ./bin/primetime
	chmod +x ./bin/chat

run-smoketest:
	./bin/smoketest

run-primetime:
	./bin/primetime

run-means:
	./bin/means


run-chat:
	./bin/chat

clean:
	rm -f ./bin/smoketest
	rm -f ./bin/primetime
	rm -f ./bin/means
	rm -f ./bin/chat
	dune clean
