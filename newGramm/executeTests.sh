#!/bin/bash

for file in ./testErrors/*
do
	echo -e
	echo "____________________ $file"
	./TestGramm.exe < "$file"
done