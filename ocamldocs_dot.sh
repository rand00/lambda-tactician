#! /bin/bash

ocamlfind ocamldoc \
    -package batteries,lwt,lwt.unix,lwt.ppx,osc,osc.lwt,re,re.pcre,lambda-term \
    -I _build/lib_sc/src/ \
    -I _build/lib_batext_rand00/src/ \
    -I _build/src/ \
    -dot \
    -d docs \
    src/ai.ml \
    src/board_array.ml \
    src/board.ml \
    src/board.mli \
    src/control.ml \
    src/gametypes.ml \
    src/gstate.ml \
    src/iinterp.ml \
    src/lambdatactician.ml \
    src/player.ml \
    src/rules.ml \
    src/rulestypes.ml \
    src/synth.ml \
    src/synth_old.ml \
    src/visualizer.ml

dot -Tpng -O ocamldoc.out \
&& feh ocamldoc.out.png


#    -hide BatExt_rand00,Pervasives \
#    lib_sc/src/*ml \
#    lib_batext_rand00/src/*.ml \
