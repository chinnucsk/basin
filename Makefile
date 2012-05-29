REBAR=./rebar
RUNN=./run_nodes.sh
DCLS=./run_remote_nodes.sh

all:
	$(REBAR) compile eunit ct

local_nodes:
	$(RUNN) ${NODES}
	killall beam.smp

dcluster:
	$(DCLS) ${HOSTS}


