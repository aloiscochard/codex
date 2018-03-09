package = codex

stack_yaml = STACK_YAML="stack.yaml"
# stack_yaml_8_4 = STACK_YAML="stack-8.4.yaml"

stack = $(stack_yaml) stack
# stack-8.4 = $(stack_yaml_8_4) stack

build:
	$(stack) build $(package)

build-dirty:
	$(stack) build --ghc-options=-fforce-recomp $(package)

run:
	$(stack) build --fast && $(stack) exec -- $(package)

install:
	$(stack) install

ghci:
	$(stack) ghci $(package):lib

test:
	$(stack) test $(package)

test-ghci:
	$(stack) ghci $(package):test:$(package)-tests

bench:
	$(stack) bench $(package)

ghcid:
	$(stack) exec -- ghcid -c "stack ghci $(package):lib --test --ghci-options='-fobject-code -fno-warn-unused-do-bind' --main-is $(package):$(package)"

dev-deps:
	stack install ghcid

.PHONY : build build-dirty run install ghci test test-ghci ghcid dev-deps

