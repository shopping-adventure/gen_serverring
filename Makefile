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

## multiple node dev
NODES = dev1 dev2 dev3 dev4
multi_start: gen_serverring
	@for n in $(NODES); do xterm -e "source ~/.zshrc; make start_$$n ; read" & done

data/%:
	mkdir -p "$@"
