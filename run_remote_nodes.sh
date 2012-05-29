#!/bin/bash


echo "'`hostname`'." > .hosts.erlang

args=("$@")

for I in $args
do
	echo "'$I'." >> .hosts.erlang
done

erl -rsh ssh -pa ./ebin/ -shared -boot start_sasl -run basin start -sname "cluster" -setcookie basin -run cluster slaves $@
