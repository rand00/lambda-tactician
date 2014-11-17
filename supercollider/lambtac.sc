
// Info about environment 
SynthDef.synthDefDir;
// Set environment
SynthDef.synthDefDir = "/home/rand/Dropbox/priv_code/games/lambda-tactician/supercollider/synthdefs";
// Visualizations - setup
FreqScope.new;
Stethoscope(s); //< only works on internal server

//-----------------------------------------------------------------------
/** Useful synths and methods **/

{ Impulse.ar(XLine.kr(800,100,5), 0.0,  0.5, 0) }.play;
//< Note: XLine moves exponentially compared to Line
//< might be useful for some intro?

{ Impulse.ar(XLine.kr(800,100,5), [0, SinOsc.kr(4,mul:1.0)],  0.3, 0) }.play;
//< Modulate phase with a Sine
//< Could be used for 'Game Over' ;D - annoying 

{ Decay2.ar( Impulse.ar([3,5]), 0.5, 0.01, SinOsc.ar([290,280], [0,0])) }.play;
//< Rounded Sine Impulses
//< sounds like some warning sound

{ Decay2.ar(
	SinOsc.ar([0.3,0.4],0,0.1) * PinkNoise.ar(10),
	0.01,
	0.03,
	SinOsc.ar([273,290], [0.5,0], 0.02) )
}.play;
//< Weird atmosphere (ghostly, old, windhowl)
// .. could add another background tone +
// .. change the freq of inp to be more irregular


{ Decay2.ar(
	SinOsc.ar([0.032,0.029],0,0.1) * PinkNoise.ar(1),
	0.01,
	0.03,
	SinOsc.ar(
		[SinOsc.kr(0.02,0.45,20)+250,
			SinOsc.kr(0.1,0,20)+300],
		[0.5,0],
		0.12) 
) }.play;
//< Weird ghostly v2

(
SynthDef( \atmos_ghostwind, {
	Out.ar( 0, Decay2.ar(
		SinOsc.ar([0.032,0.029],0,0.1) * PinkNoise.ar(1),
		0.01,
		0.03,
		SinOsc.ar(
			[SinOsc.kr(0.02,0.45,20)+250, SinOsc.kr(0.1,0,20)+300],
			[0.5,0],
			0.12) 
	))
}).writeDefFile;
)
//< Weird ghostly v2 : synthdef 


{ Decay2.ar(
	SinOsc.ar([0.032,0.029],0,0.1),
	0.01,
	0.03,
	SinOsc.ar(
		[SinOsc.kr(0.02,0.45,20)+250,
			SinOsc.kr(0.1,0,20)+300],
		[0.5,0],
		0.12) 
) }.play;
//< Weird synthly ghost 1
//.. maybe add som randomness to inp frequencies.. 

{ Decay2.ar(
	SinOsc.ar( [Rand(0.032,0.015), Rand(0.021,0.029)],
		0,0.1),
	0.01,
	0.03,
	SinOsc.ar(
		[SinOsc.kr(0.02,0.45,20)+250, SinOsc.kr(0.1,0,20)+300],
		[0.5,0],
		0.12) 
) }.play;
//< Weird synthly ghost 2 - more random


{ Decay2.ar(
	SinOsc.ar([0.11,0.2345],0,0.1),
	0.01,
	0.03,
	SinOsc.ar([273,290], [0.5,0], 0.01) ) * PinkNoise.ar(10)
}.play;
//< Ocean like.. with a hint of sine


{ Decay2.ar(
	SinOsc.ar([0.11,0.2345],0,0.1),
	0.01,
	0.03,
	PinkNoise.ar(10,mul:0.08))
}.play;
//< More like Ocean









