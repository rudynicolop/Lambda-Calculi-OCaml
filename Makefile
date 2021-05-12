.PHONY: all build run clean

all: build

default: build

build:
	@dune build

run:
	make all
	dune exec -- ./bin/main.exe

clean:
	@dune clean
