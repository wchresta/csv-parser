.PHONY: all build install clean test

all: build

build/exec/runTest: Test/*.idr Language/*.idr
	idris2 --build test-csv-parser.ipkg

test: build/exec/runTest
	./build/exec/runTest

build: Language/*.idr
	idris2 --build csv-parser.ipkg

install:
	idris2 --install csv-parser.ipkg

clean:
	rm -rf ./build


