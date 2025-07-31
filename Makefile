.PHONY: all format build test lint run clean watch

all: format build test

format:
	@fourmolu -q -i \
		app/Main.hs \
		src/App.hs \
		src/Database.hs \
		src/Domain.hs \
		src/Env.hs \
		src/Handler.hs \
		src/Logger.hs \
		src/Service.hs \
		src/State.hs \
		test/Spec.hs

build:
	@stack build

test:
	@stack test

lint:
	@hlint src/*.hs app/*.hs

run:
	@stack run

clean:
	@stack purge
	@rm -rf dist-newstyle

watch:
	ghciwatch --clear
