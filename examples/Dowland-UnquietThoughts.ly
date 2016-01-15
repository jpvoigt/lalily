\version "2.19.32"

\include "lalily.ly"

\optionsInit opts
\optionsAdd opts element.choir.template \Path lalily.vocal.group
\optionsAdd opts element.choir.staff-mods \with { midiInstrument = "choir aahs" }
%\optionsAdd opts element.accomp.template \Path lalily.piano

\setDefaultTemplate mutopia.DowlandJ.UnquietThoughts lalily.group #opts
\setTitle "Unquiet Thoughts your civil slaughter stint"
\setComposer #'("John Dowland" . "(1563-1626)")
\setSubSubTitle "From \"The First book of Songes or Ayres\""
\setSubTitle "Ayres and Lute Songs I"
\setHeader #'composition "1597"
\setHeader #'copyright "Public Domain"


\putMusic meta {
	\key g \minor
	\time 2/1
	\skip 1*16
	\repeat volta 2 {
	\skip 1*7
	} \alternative {{\skip 1}{\skip 1 \bar "|."}}
}

\putMusic choir.sop.music \relative c'' {
	d2 d4 d4 es1|
	r4 d2 c bes4 a4. a8 |
	g2 r4 a bes c d2 |
	es4 d2 c4 d bes a2 |
%5
	r2 r4 c4 bes g bes2 |
	a4 d4. c8( bes4) c c d2 |
	r4 a4 bes d2 c2 g4 |
	fis bes a4. a8 g1 |
	%\repeat volta 2 {
		r4 d'2 bes d4 c a |
%10
		bes g bes c d2 r4 a4 |
		c c g g bes bes f f |
		bes4 g a a
	%} \alternative {
	  {  g1  }  
	  {  g1  }
	%}
}

\putMusic choir.sop.lyrics \lyricmode{
	Un -- qui -- et thoughts 
	your ci -- vill slaugh -- ter 
	stint
	and wrap your wrongs 
	with -- in a pen -- sive hart: 
	And you my tongue 
	that maks my mouth a minte, 
	and stamps my thoughts to 
	coyne them words by arte: 
%	\repeat volta 2 {
		Be still for if you 
		ev -- er doo the like,
		Ile 
		cut the string, Ile cut the string, that 
		maks the ham -- mer
%	} \alternative {
%	{
		 strike. 
%	}{
		 strike. 
%	}}
}
	

\putMusic choir.alt.music \relative c''{
	bes2 bes4 bes g1 |
	d2 d4 e fis( g2) fis4 |
	g2 r4 fis4 g a bes4. bes8 |
	a4 fis g4. g8 fis4 r8 g a4 c |
%5
	bes g g fis g4. g8 d2 |
	f4 bes4. a8 g4 a8 bes4 a8 bes4 r8 bes |
	a4 fis g4. fis8  d8([ e] f4) es2 |
	d d4.( c8) b2 r4 g'4 |
	%\repeat volta 2 {
		fis4 a d, g f2. c4 |
		d d g4. g8 fis2 r4 c4 |
		c es es bes d d d d |
		d c4. a8 d4
	%} 
	%\alternative { 
		{  b2 r4 g'4 } 
		{  b,1 }
	%} 
}
\putMusic choir.alt.lyrics \lyricmode {
	Un -- qui -- et thoughts, 
	your ci -- vil slaugh -- ter 
	stint 
	and wrap your wrongs with -- 
	in a pen -- sive hart, and you my 
	toung that makes my mouth a minte, 
	my toung that makes my mouth a
 	minte, and
	stamps my thoughts to coine __  them
	 words by __  art 
	be
% \repeat volta 2 {
	 	 still be still for if you
		 ev -- er do the like
		 Ile |
		 cut the string Ile cut the string that |
		 makes the  ham -- mer
%	} \alternative {
%	{
		strike.  Be 
%	} { 
		strike.
%	}}
}

\putMusic choir.ten.music \relative c' {
	f2 g4  f8[ bes,] bes1 |
	bes2. a2 g4 d'4. c8 |
	b2 r4 d4 d f f4. d8 |
	c c bes4. a8( g4) a r8 bes c4 a |
%5
	d2 r4 bes4 d2 r4 bes4 |
	c g  bes8[ c d bes] f'4 g,2 r8 bes8 |
	c4 d g,4. a8 bes4  a8[ bes] c4. bes8 |
	a4 g g fis g2 es'2  |
	%\repeat volta 2 { 
		d4. c8  bes[ a] g4 bes2 a4 f4 ~ |
		f4 bes4.( a8 g4) a a a a |
		g g g g f f bes( a4 ~ |
		a8) g8 g2 fis4 
	%} 
	%\alternative{ 
		      { g2 es'2  } 
		      { g,1  } 
	%}
}
\putMusic choir.ten.lyrics \lyricmode{
	Un qui -- et thoughts, 
	your civ -- ile slaugh -- ter 
	stint,  and wrap your wrongs with -- 
	in a pen -- sive hart  and you my 
	tonge  my tonge that 
	makes my mouth __   a mint, and 
	stampes my thoughts, my thoughts to __  coine to 
	coin them words by  art, be2 
%	\repeat volta 2 {
		still for if you ev -- er do the __
		like ile cut the  string  
		ile cut the string that makes the __ % should be the2 
		ham --  mer 
%	} \alternative {
		{ strike. Be  }
		{ strike.   }
%	}
}

\putMusic choir.bas.music \relative c' {
	bes2 g4 bes es,1 |
	g2 d d d4 d |
	g,2 r4 d'4 g f bes bes, |
	c d es4. es8 d4 g2 fis4 |
	g bes a4. a8 g2 r4 g4 |
	f d g2 f bes,2 |
	r1 r4 f' c c| 
	d2. d4 g,2 r2 
	%\repeat volta 2 {
		r1 r2 f'2 |
		d4 g4.( f8) es4 
		d4 d  f  f| c c es es 
		bes bes  d  d |g es d d
        %}
	%\alternative{
	{g2 r2}
	{g1}
	%}
}

\putMusic choir.bas.lyrics \lyricmode{
	Un -- qui -- et thoughts,
	your civ -- ile slaugh -- ters
	stint,  and wrap your wrongs with --
	in a pens -- ive hart, wrongs with --
	in a pens -- ive hart,  that
	makes my mouth a mint
	to coine them
	words by 
	arte,
%	\repeat volta 2 {
		 ev -- 
		er do __  the like, Ile cut the 
		string, Ile cut the string, the string that 
		makes the ham -- mer
%	} \alternative { 
		{ strike.  }
		{ strike.  } 
%	}
}

\lalilyTest

