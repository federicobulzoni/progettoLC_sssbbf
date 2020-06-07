#!/bin/bash

for file in ./test/corretti/*
do
	echo -e
	echo "____________________ $file"
	./TestGramm.exe < "$file"
done