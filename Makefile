build: build-7.10

build-7.10:
	STACK_YAML="stack-7.10.yaml" stack build

build-8.0:
	STACK_YAML="stack-8.0.yaml" stack build

build-8.2:
	STACK_YAML="stack-8.2.yaml" stack build

build-8.4.2:
	STACK_YAML="stack-8.4.yaml" stack build
