#!/bin/bash

for I in `seq 1 20`
do
	erl -pa ./ebin/ -boot start_sasl -run basin start -sname "basin-"$I -noshell -noinput &
done

erl -pa ./ebin/ -boot start_sasl -run basin start -sname "basin-0"
