.PHONY: main.exe test

main.exe:
	dune build main.exe

test: main.exe
	DYLD_LIBRARY_PATH=`opam config var z3:lib` \
	_build/default/main.exe test/test1.tiny
	DYLD_LIBRARY_PATH=`opam config var z3:lib` \
	_build/default/main.exe test/test2.tiny
