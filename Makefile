build: ## Build
	@cabal build all

clean: ## Clean
	@cabal clean

repl: ## Start a REPL
	@cabal repl acl

test: ## Run the tests
	@cabal test

lint: ## Run hlint
	@find -name "*.hs" | xargs -P $(PROCS) -I {} hlint --refactor-options="-i" --refactor {}

style: ## Run formatter
	@fourmolu src test -q --mode inplace

tags: ## Run ghc-tags
	@ghc-tags -c src test

help:
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | awk 'BEGIN {FS = ":.* ?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

UNAME := $(shell uname)

SHELL := /usr/bin/env bash

ifeq ($(UNAME), Darwin)
	PROCS := $(shell sysctl -n hw.logicalcpu)
else
	PROCS := $(shell nproc)
endif

.PHONY: all $(MAKECMDGOALS)

.DEFAULT_GOAL := help
