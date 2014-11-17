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
//< synthly ghost - add Rand to inp freqs 