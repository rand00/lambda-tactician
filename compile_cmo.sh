#! /bin/bash

ocamlbuild -use-ocamlfind -no-links -Is core_rand \
    gametypes.cmo visualizer.cmo board.cmo player.cmo control.cmo ai.cmo iinterp.cmo #rules.cmo
