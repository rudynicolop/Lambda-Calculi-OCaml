.PHONY: all build run clean

NAME = lambda

all: build

default: build

build:
	@dune build
# bin/main.exe
# @cp ./_build/default/bin/main.exe

run:
	make all
	dune exec -- $(NAME)

clean:
	@dune clean
# @rm -f $(VANILLA)
