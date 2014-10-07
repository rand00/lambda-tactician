
(*- batteries loaded with '.ocamlinit'
  - this script is to be run from source-dir*)

(*Core_rand00 dependency*)
Sys.chdir ((Sys.getcwd()) ^ "/_build/core_rand");;
#load "otext.cmo";;
#load "list_rand.cmo";;
#load "file_rand.cmo";;
#load "sys_rand.cmo";;
#load "arg_rand.cmo";;
#load "core_rand00.cmo";;

Sys.chdir "..";;
(*lambdatactician modules*)
#load "gametypes.cmo";;
#load "visualizer.cmo";;
#load "ai.cmo";;
#load "control.cmo";;
#load "iinterp.cmo";;
