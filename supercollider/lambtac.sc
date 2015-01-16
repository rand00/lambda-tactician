
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

{ Impulse.ar(XLine.kr(200,10,5), [0, SinOsc.kr(4,mul:1.0)],  0.3, 0) }.play;
//< goto: maybe build on this and add reverb?

{ Decay2.ar( Impulse.ar([3,5]), 0.5, 0.01, SinOsc.ar([290,280], [0,0])) }.play;
//< Rounded Sine Impulses
//< sounds like some warning sound
{ Decay2.ar(Impulse.ar(1), 0.001, LFSaw.kr(0.3, 0, -0.3, 0.3)) }.plot

{ var freq,sig;
	freq = 69;
	sig = Decay2.ar(
		Impulse.ar(8, 0,LFSaw.kr(0.3, 0, -0.1, 0.1)),
		0.001, 
		0.3, 
		Mix.ar(Pulse.ar([freq,freq+1], 0.3)));
	[ sig, sig ] * 0.3
}.play

{ var freq,sig;
	freq = 69;
	sig = Impulse.ar(8, 0,LFSaw.kr(0.3, 0, -0.1, 0.1)) *
	Mix.ar(Pulse.ar([freq,freq+1], 0.3));
[sig, sig ] 
}.play

{ var in = MouseX.kr(20,800); Pulse.ar([in,in],0.8,0.08 ) }.play 
//< interesting sound as base - resembles the vangelis base vco a bit

{ LFSaw.kr(0.3, 0, -0.2, 0.3) }.plot(3)

{ Impulse.ar(8, 0,LFSaw.kr(0.3, 0, -0.3, 0.3)) }.plot

{ LFSaw.ar([30,30], 0, -0.01, 0.3) }.play
//< goo

{ Impulse.ar(8, 0) }.plot

{ var freq = 69;
	Decay2.ar(
		Impulse.ar(8, 0),
		0.001, 
		0.021) }.plot

{ var freq = 69; Mix.ar(Pulse.ar([freq,freq+1], 0.3)) }.plot
{ var freq = 69; Pulse.ar([freq,freq+1], 0.3) }.plot
{ var freq = 69; Pulse.ar([freq,freq+1], 0.8) }.plot


{ Decay2.ar(Impulse.ar(XLine.kr(1,50,20), 0.25), 0.01, 0.4, FSinOsc.ar(600)) * 0.3 }.play;
// goo

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


SynthDef( 
	\ratata, 
	{| panfrom = -1, mul = 0.3 |
		var sig,dur,panline,freq;
		dur = 1.7;
		freq = Rand(60, 90);
		panline = Line.kr(panfrom, panfrom * -1, dur);
		FreeSelfWhenDone.kr( panline );
		sig = Pan2.ar(
			Decay2.ar(
				Impulse.ar(8, 0,LFSaw.kr(0.3, 0, -0.3, 0.3)),
				0.001, 
				0.3, 
				Mix.ar(Pulse.ar([freq,freq+1], 0.3))),
			panline) * mul;
		Out.ar( [0,1], sig);
	}
).writeDefFile;//add;
//< for moving piece ; pass panfrom -1/1 

x = Synth(\ratata, [panfrom:-1]);


SynthDef( 
	\ratata2, 
	{| panfrom = -1, mul = 0.3 |
		var sig,dur,panline,freq;
		dur = 1.7;
		freq = Rand(60, 90);
		panline = Line.kr(panfrom, panfrom * -1, dur);
		FreeSelfWhenDone.kr( panline );
		sig = Pan2.ar(
			Decay2.ar(
				Impulse.ar(8, 0,Line.kr(0.6,0,dur)),
				0.001, 
				0.3, 
				Mix.ar(Pulse.ar([freq,freq+1], 0.3))),
			panline) * mul;
		Out.ar( [0,1], sig);
	}
).add;//add;
//< for moving piece ; pass panfrom -1/1 

x = Synth(\ratata2, [panfrom:-1]);

{ z = Mix.ar(Pulse.ar([80,81], 0.3)); [z, z] * 0.2 }.play

// scifi transport passing by >
{ 
	var freq,sig,amp,dur,free,pan_dur, pan_start, a, cycle, slide, bfreq;
	amp = 0.3;
	dur = 2.6;
	//base tones
	bfreq = Rand(470,180);
	slide = Rand(-12,12);
	freq = [bfreq + slide, bfreq] + (SinOsc.kr(10) * 10);
	sig = LPF.ar( LFSaw.ar(freq, 0, SinOsc.kr(0.2)), 1000);
	//reverb
	sig = FreeVerb.ar(
		sig, 0.7, 0.7, 0.4);
	a = [-1, 1];
	//envelope
	//sig = Decay2.ar( sig, 0.1, 0.3);
	//panning
	pan_start = Rand(-1,1); 
	pan_dur = Line.kr(pan_start, pan_start * -1, dur);
	FreeSelfWhenDone.kr( pan_dur );
	sig = Pan2.ar( sig, pan_dur );
	sig = sig * amp;
	Out.ar([0,1], sig);
}.play;

// scifi transport :: mousex ctrl (play around) (makey makey)
{ 
	var freq,sig,amp,dur,free,pan_dur, pan_start, a, cycle, slide, bfreq;
	amp = 0.3;
	dur = 2.6;
	//base tones
	bfreq = Rand(470,180) + MouseX.kr(-180,500);
	slide = Rand(-12,12);
	freq = [bfreq + slide, bfreq] + (SinOsc.kr(10) * 10);
	sig = LPF.ar( LFSaw.ar(freq, 0, SinOsc.kr(0.2)), 1000);
	//reverb
	sig = FreeVerb.ar(
		sig, 0.7, 0.7, 0.4);
	a = [-1, 1];
	//envelope
	//sig = Decay2.ar( sig, 0.1, 0.3);
	//panning
	pan_start = Rand(-1,1); 
	pan_dur = MouseX.kr(-1,1);
	//pan_dur = Line.kr(pan_start, pan_start * -1, dur);
	//FreeSelfWhenDone.kr( pan_dur );
	sig = Pan2.ar( sig, pan_dur );
	sig = sig * amp;
	Out.ar([0,1], sig);
}.play;


// >> synthlyghost - bg tone
SynthDef( 
	\synth_ghost2, 
	{| mul=0.02 |
		Out.ar(
			[0, 1],
			{ Decay2.ar(
				SinOsc.ar( [Rand(2.1,0.015), Rand(3,0.029)],
					0,0.1),
				0.01,
				0.03,
				SinOsc.ar(
					[SinOsc.kr(0.002,0.45,20)+250, SinOsc.kr(0.001,0,20)+300],
					[0.5,0],
					0.12)
			) } * mul
		)
	}
).add;//writeDefFile;//add; 

x = Synth(\synth_ghost2);

