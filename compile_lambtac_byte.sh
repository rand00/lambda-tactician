#! /bin/bash

#important to use -Is instead of -I
ocamlbuild -use-ocamlfind -cflag -bin-annot \
    -Is core_rand \
    lambdatactician.byte
