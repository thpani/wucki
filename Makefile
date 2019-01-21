.PHONY: main.exe test

main.exe:
	dune build main.exe

test: main.exe
	_build/default/main.exe test/test1.tiny
	_build/default/main.exe test/test2.tiny
