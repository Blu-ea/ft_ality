PACKAGE := ft-ality
EXE := ft-ality
CABAL := cabal

.PHONY: all build run repl clean help

all: build

build:
	$(CABAL) build

run: build
	$(CABAL) run $(EXE) -- $(ARGS)

clean:
	$(CABAL) clean

help:
	@echo "Usage: make [target]"
	@echo
	@echo "Targets:"
	@echo "  build    Build the project with cabal"
	@echo "  run      Build then run the executable (set ARGS for arguments)"
	@echo "  clean    Clean build artifacts (also removes dist-newstyle)"
	@echo "  help     Show this help"


