#! /bin/bash

#important to use -Is instead of -I
ocamlbuild -use-ocamlfind -cflag -bin-annot \
    -Is core_rand,src \
    lambdatactician.byte
