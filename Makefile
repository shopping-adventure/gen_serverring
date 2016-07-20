.SECONDARY:

all: deps gen_serverring

deps: 
	@mix do deps.get

gen_serverring:
	@mix compile

## single node dev
start: gen_serverring
	@iex -S mix run

start_%: config/%.exs data/%
	@iex --name $*@127.0.0.1 -S mix run

data/%:
	mkdir -p "$@"
