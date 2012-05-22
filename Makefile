REBAR=./rebar
RUNN=./run_nodes.sh

all:
	$(REBAR) compile eunit ct

local_nodes:
	$(RUNN) ${NODES}
	killall beam.smp

