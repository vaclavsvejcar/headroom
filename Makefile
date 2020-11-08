.PHONY: clean
clean:
	rm -rf ./.stack-work/
	rm -rf ./dist-newstyle

.PHONY: dist
dist: fresh
	stack sdist

.PHONY: pretty
pretty: 
	find ./app -name '*.hs' | xargs stylish-haskell -i -v
	find ./src -name '*.hs' | xargs stylish-haskell -i -v
	find ./test -name '*.hs' | xargs stylish-haskell -i -v
	find ./app -name '*.hs' | xargs brittany --write-mode=inplace
	find ./src -name '*.hs' | xargs brittany --write-mode=inplace
	find ./test -name '*.hs' | xargs brittany --write-mode=inplace

.PHONY: fresh
fresh: clean build

.PHONY: build
build: pretty
	stack build
	stack test
	stack haddock