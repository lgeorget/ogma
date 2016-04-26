.PHONY: api server all client

default: all

api:
	@(echo "building: $@")
	@(stack build --stack-yaml=api/stack.yaml)

server:
	@(echo "building: $@")
	@(stack build --stack-yaml=server/stack.yaml)

client:
	@(echo "building: $@")
	@(stack build --stack-yaml=client/stack.yaml)

all: api server client

clean:
	@(stack clean --stack-yaml=client/stack.yaml)
	@(stack clean --stack-yaml=api/stack.yaml)
	@(stack clean --stack-yaml=server/stack.yaml)
