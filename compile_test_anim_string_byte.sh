#! /bin/bash

ocamlbuild -use-ocamlfind -cflag -bin-annot \
    -Is src,lib_batext_rand00/src,lib_sc/src \
    experiments/anim_string.byte
