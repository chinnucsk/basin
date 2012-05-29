#!/bin/bash


echo "'`hostname`'." > .hosts.erlang

for I in `seq 1 $1`
do
	erl -pa ./ebin/ -boot start_sasl -run basin start -sname "basin-"$I -detached -noinput -setcookie basin &
done


erl -pa ./ebin/ -boot start_sasl -run basin start -sname "basin-0"
