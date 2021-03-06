#! /bin/bash
#/bin/sh

## This script is based off of the following guide;
## http://swiki.hfbk-hamburg.de/MusicTechnology/634 

# cd $HOME/cvs/SuperCollider3/build/ #< why neccesary?

SCSYNTH=$(which scsynth)
PORT=57110
#PORT=57111

#export SC_JACK_INPUTS=2
#export SC_JACK_OUTPUTS=2

if [[ $SC_JACK_DEFAULT_INPUTS == "" ]]
then 
    #export SC_JACK_DEFAULT_INPUTS="alsa_pcm:capture_1,alsa_pcm:capture_2"
    export SC_JACK_DEFAULT_INPUTS="system"
fi

if [[ $SC_JACK_DEFAULT_OUTPUTS == "" ]]
then 
    #export SC_JACK_DEFAULT_OUTPUTS="alsa_pcm:playback_1,alsa_pcm:playback_2"
    export SC_JACK_DEFAULT_OUTPUTS="system"
fi

export SC_SYNTHDEF_PATH="./supercollider/synthdefs" 
#< this get's loaded, but is not set as standard when writing synths

# like this it's not necessary to run this script as root < (because of being in build-dir?)
# and it shows you how the server is actually started
SCCMD="$SCSYNTH -i ${SC_JACK_INPUTS} -o ${SC_JACK_OUTPUTS} -u $PORT $@"
echo $SCCMD "$@"

$SCCMD 2>&1

#nice -n -10 $SCCMD || $SCCMD

