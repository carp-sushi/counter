# Haskell counter service build targets
.PHONY: all format build test lint run clean watch

# Source files for formatting and linting
HS_FILES = app/*.hs src/*.hs src/**/*.hs test/*.hs

all: format build test

format:
	@fourmolu -q -i $(HS_FILES)

build:
	@stack build

test:
	@stack test

lint:
	@hlint $(HS_FILES)

run:
	@stack run

clean:
	@stack purge
	@rm -rf dist-newstyle

watch:
	@ghciwatch --clear
