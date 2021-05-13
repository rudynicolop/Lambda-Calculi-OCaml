.PHONY: all build run clean

all: build

default: build

build:
	@dune build

clean:
	@dune clean
