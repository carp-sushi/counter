.PHONY: all format build test lint run clean watch

all: format build test

format:
	@fourmolu -q -i \
		app/*.hs \
		src/*.hs \
		src/**/*.hs \
		test/*.hs

build:
	@stack build

test:
	@stack test

lint:
	@hlint src/*.hs src/**/*.hs app/*.hs

run:
	@stack run

clean:
	@stack purge
	@rm -rf dist-newstyle

watch:
	ghciwatch --clear
