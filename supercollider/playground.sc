
//-----------------------------------------------------------------------
/** sending and receiving osc msg's (from osc server to osc server) (from oscdef)
	-> I need to start a server @ ocaml if I want to msg super-collider
**/
n = NetAddr("127.0.0.1", 57120); // local machine
OSCdef(\test, {|msg, time, addr, recvPort| \unmatching.postln}, '/chat', n); // def style

m = NetAddr("127.0.0.1", 57120); // loopback
m.sendMsg("/chat", "Hello App 1");


//-----------------------------------------------------------------------
/** Random shit from other sources ? >> delete on the fly when done **/

{ Decay2.ar(
	SinOsc.ar([0.11,0.2345],0,0.1),
	0.01,
	0.03,
	PinkNoise.ar(10,mul:0.08))
}.play;
	// goto

play({ Decay.ar(Impulse.ar(XLine.kr(1,50,20), 0.25), 0.2, PinkNoise.ar, 0) });

plot({ Decay2.ar(Impulse.ar(1), 0.001, 0.01) })
//< does this work on internal serveR?
{ Decay2.ar(Impulse.ar(1), 0.001, 0.01) }.scope
//< also works on external server

{
	var d = XLine.kr(800,100,5);
	Impulse.ar(d, [0, SinOsc.kr(4,mul:1.0)],  0.3, 0);
	FreeSelf.kr((d)-400);
}.play;


{
	var d = XLine.kr(800,400,1);
	FreeSelfWhenDone.kr(d);
	Decay2.ar (
		Impulse.ar(d, [0, SinOsc.kr(10,mul:1.0)],  0.3, 0) )
}.play;


{ Impulse.ar(XLine.kr(1,50,20), 0.25) }.play
{ FSinOsc.ar(600)}.play
{ Impulse.ar(XLine.kr(1,50,20), 0.25) * Decay2.ar}.play
{ Decay2.ar(Impulse.ar(XLine.kr(1,50,20), 0.25), 0.01, 0.2, FSinOsc.ar(600)) }.play;

{ Decay2.ar(
	Impulse.ar(XLine.kr(1,20,3), 0.25), //input signal (when to attack/decay)
	0.4, //attack time (controls mul)
	0.1, //decay time (controls mul)
	FSinOsc.ar(600)) // add
}.play;
//< goto

{ Decay2.ar( Impulse.ar([3,5]), 0.5, 0.01, SinOsc.ar([290,280], [0,0])) }.play;

{ Decay.ar(Impulse.ar(XLine.kr(1,50,20), 0.25), 0.2, FSinOsc.ar(600), 0)  }.play;

{ Impulse.ar(XLine.kr(800,100,5), [0, SinOsc.kr(4,mul:1.0)],  0.3, 0) }.play;

{
Out.ar(
	[0,1],
	{ 
		var freq = 90;
		var bmul = 0.1;
		SinOsc.ar(freq*3, 0, bmul * 2 )
		- Impulse.ar( freq, 0, bmul )
	} * 0.5
)}.play

{ SinOsc.ar(180,0,1) }.play
//goto
( 
SynthDef( \base_tone, {arg freq=180 ;
	{ Out.ar( 
		[0,1],
		Impulse.ar( freq, 0, 0.3)
	)}
})).add;

x = Synth(\base_tone);
//goto


{ XLine.ar(800,100,0.1) }.play


//.. atmosphere play

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



(
SynthDef( \synth_ghost, { arg mul=0.05;
	Out.ar(
		[0, 1],
		{ Decay2.ar(
			SinOsc.ar( [Rand(0.032,0.015), Rand(0.021,0.029)],
				0,0.1),
			0.01,
			0.03,
			SinOsc.ar(
				[SinOsc.kr(0.02,0.45,20)+250, SinOsc.kr(0.1,0,20)+300],
				[0.5,0],
				0.12)
		) } * mul
	)
})).add//writeDefFile;//add; 
//< synthly ghost - add Rand to inp freqs 

// make this the tmp background tone of lambdatac
x = Synth(\synth_ghost);
x.set(\mul, 0.01);

SynthDef( \synth_ghost1, { arg mul=0.05;
	Out.ar(
		[0, 1],
		{ Decay2.ar(
			SinOsc.ar( [Rand(100,0.015), Rand(40,0.029)],
				0,0.1),
			0.01,
			0.03,
			SinOsc.ar(
				[SinOsc.kr(0.02,0.45,20)+250, SinOsc.kr(0.1,0,20)+300],
				[0.5,0],
				0.12)
		) } * mul
	)
}).writeDefFile;//writeDefFile;//add; 

x = Synth(\synth_ghost1);


SynthDef( \synth_ghost2, { arg mul=0.02;
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
}).add;//writeDefFile;//add; 

x = Synth(\synth_ghost2);

//---- from sc paper
(
SynthDef(
	\analog_synth,
	{| gate=1, freq, amp, sustain |
		var sig, env;
		sig = LFSaw.ar(freq);
		sig = LPF.ar(sig, 4000);
		env = EnvGen.kr(Env.adsr(0.01, 0.3, 0.5, 1.0),
			gate, 1, sustain, 2);
		sig = sig * env * amp;
		Out.ar([0,1], sig);
	})
).add;

x = Synth( \analog_synth ) 

// scifi transport passing by
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


(

// example signal to process

(
Synth({
	var z;
	z = Decay2.ar(
		Impulse.ar(8, LFSaw.kr(0.25, -0.6, 0.7)), 
		0.001, 0.3, FSinOsc.ar(500));
}, 0.8)
).play;

{
	Decay2.ar(
		Impulse.ar(8, LFSaw.kr(0.25, -0.6, 0.7)), 
		0.001, 0.3, FSinOsc.ar(500))
}.play

(
{
	var z = Decay2.ar(
		Impulse.ar(8, LFSaw.kr(0.25, -0.6, 0.7)), 
		0.001, 0.3, FSinOsc.ar(500));
	[z, Normalizer.ar(z, 0.4, 0.01)]
}.play
)

(
// limiter << nice one!
play({
    var z;
    z = Decay2.ar(
        Impulse.ar(8, 0,LFSaw.kr(0.3, 0, -0.3, 0.3)),
        0.001, 0.3, Mix.ar(Pulse.ar([80,81], 0.3)));
    Compander.ar(z, z,
        thresh: MouseX.kr(0.1, 1),
        slopeBelow: 1,
        slopeAbove: 0.1,
        clampTime: 0.01,
        relaxTime: 0.01
    );
	[z, z];
})
)

//goto
{ 
	Pan2.ar(
		Decay2.ar(
			Impulse.ar(8, 0,LFSaw.kr(0.3, 0, -0.3, 0.3)),
			0.001, 
			0.3, 
			Mix.ar(Pulse.ar([80,81], 0.3))),
		SinOsc.kr(0.2,mul:0.65)) * 0.3
}.play

{ Impulse.ar(8, 0,LFSaw.kr(0.3, 0, -0.3, 0.3)) }.play

(
Synth.play({
    var z;
    z = Decay2.ar(
        Impulse.ar(8, LFSaw.kr(0.25, -0.6, 0.7)),
        0.001, 0.3, FSinOsc.ar(500));
    [z, Normalizer.ar(z, 0.4, 0.01)]
}, 0.5)
)

2.floor;


(
SynthDef("help-Select",{ arg out=0;
	var a,cycle;
	a = [
			SinOsc.ar,
			Saw.ar,
			Pulse.ar
		];
	cycle = a.size  * 0.5;
	Out.ar(out,
		Select.ar(LFSaw.kr(1.0,0.0,cycle,cycle),a) * 0.2
	)
}).play;
)

(
SynthDef("help-Select-2",{ arg out=0;
	var a,s,cycle;
	a = Array.fill(32,{ rrand(30,80) }).midicps;
	a.postln;
	cycle = a.size  * 0.5;
	s = Saw.ar(
			Select.kr(
				LFSaw.kr(0.02,0.0,cycle,cycle),
				a
			),
			0.18
	);
	Out.ar([0,1],s*0.3 )
}).play;
)

{ LFSaw.ar(30,0.0,mul:0.01) }.play

{ Resonz.ar(WhiteNoise.ar(0.5), 2000, 0.1) }.play

    //env = Env([0, 1, 0.5, 1, 0], [0.01, 0.5, 0.02, 0.5]);
	//env = Env.adsr(0.01, 0.3, 0.5, 1.0);
	//	env = Env.new([0, 1, 0.9, 0], [0.1, 0.5, 1],[-5, 0, -5]);
	//env = EnvGen.kr(env, 1, 1, 1, 10, 2);
	//env = EnvGen.kr( env, doneAction: 2);
	//env = EnvGen.kr( env);


(
{ var env;
env = Line.kr(0, 1, 1);
	FreeSelfWhenDone.kr(env); // free synth at end of line
SinOsc.ar(200, 0, 0.5) * env
}.play;
)

Env([0, 1, 0.5, 1, 0], [0.01, 0.5, 0.02, 0.5]).plot;
Env.adsr(0.01, 0.3, 0.5, 1.0).plot;
Env.new([0, 1, 0.9, 0], [0.1, 0.5, 1],[-5, 0, -5]).plot;
Env.perc.plot
Env.adsr.plot
Env.new.plot



Filter.dumpClassSubtree

(
{
	var env = Env([0, 1, 0.5, 0.8, 0, 1.2, 0], [0.01, 0.5, 0.02, 0.5, 0.2, 0.5]);
	var gate = Impulse.kr(MouseX.kr(0.2, 3), 0.5);
	var gen = EnvGen.kr(env, gate);
	SinOsc.ar(270, SinOsc.ar(gen * 473)) * gen * 0.2
}.play
)


(
{
	var env = Env([0, 1, 0.5, 1, 0], [0.01, 0.5, 0.02, 0.5]);
	SinOsc.ar(470) * EnvGen.kr(env, doneAction: 2)
}.play
)


(
{
	var env = Env([0.0, [-0.2, 0.5], 0.0, 1.0, [-0.4, 0.9], 0.0], [0.05, 0.1, 0.01, 1.0, 1.5], -4);
	var envgen = EnvGen.ar(env, Dust.ar([1, 1]));
	SinOsc.ar(
		envgen * 440 + 550
	) * envgen * 0.1
}.play
);


{ PinkNoise.ar(EnvGen.kr(Env.perc, doneAction: 2)) }.play

x = Synth( \analog_synth, [gate:0, freq:100, amp:0.5, sustain:0])
