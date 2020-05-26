#!/bin/bash

for file in ./test/*
do
	echo -e
	echo "____________________ $file"
	./TestGramm.exe < "$file"
done