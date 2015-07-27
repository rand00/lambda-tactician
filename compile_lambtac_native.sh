#! /bin/bash

#important to use -Is instead of -I

ocamlbuild -use-ocamlfind -cflag -bin-annot \
    -Is src,lib_batext_rand00/src,lib_sc/src \
    lambdatactician.native
