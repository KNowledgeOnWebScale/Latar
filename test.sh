#!/bin/bash
for dir in alpha/* beta/*
do
    pushd "${dir}"
    ./test 
    popd
done
