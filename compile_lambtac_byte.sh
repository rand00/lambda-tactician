#! /bin/bash

#important to use -Is instead of -I
ocamlbuild -use-ocamlfind \
    -Is src,lib_batext_rand00/src,lib_sc/src \
    lambdatactician.byte
#    -cflag -bin-annot \
#    -cflag -g \
