%%%% This file is part of lalily, an extension to lilypond <http://www.lilypond.org/>.
%%%%
%%%% Copyright (C) 2011--2013 Jan-Peter Voigt <jp.voigt@gmx.de>
%%%%
%%%% lalily is free software: you can redistribute it and/or modify
%%%% it under the terms of the GNU General Public License as published by
%%%% the Free Software Foundation, either version 3 of the License, or
%%%% (at your option) any later version.
%%%%
%%%% lalily is distributed in the hope that it will be useful,
%%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%%% GNU General Public License for more details.
%%%%
%%%% You should have received a copy of the GNU General Public License
%%%% along with lalily.  If not, see <http://www.gnu.org/licenses/>.

\version "2.18.0"
% include "lalily.ly" from folder above
\include "../lalily.ly"

% change current music-folder (like "cd jan-peter/alte-hits/choral" on the command-line)
\setMusicFolder jan-peter.alte-hits.choral
% set default template for this folder to lalily.vocal.group
\setTemplate lalily.vocal.group
% we set no options here - SATB is default

% set a title
\setTitle "Herzlichen Glückwunsch"
% register and set composer (you use an external file for all you needed composers!)
\registerPerson #'jan-peter.voigt "Jan-Peter Voigt" "(*1972)"
\setComposer #'jan-peter.voigt
% set header "composition"
\setHeader #'composition "(1988)"

% "meta"-track of music: C major, 4/4, 24 measures
\putMusic meta {
  % midiTempo is a utility-function to only set the midi-tempo
  \midiTempo 125/4
  \time 4/4
  \key c \major
  s1*24
  \bar "|."
}
% music/melody track/voice for sopranos
% with all musical information only relevant for this voice
\putMusic sop.music \relative c'' {
  e2 f4 g | e8([ g]) f([ e]) d([ c]) b([ a]) | c4.( d8) e4 d8 c | d2 f4 \breathe f |
  e4.( d8) c4 \breathe a8 a | a2 g | a' g~ | g2. \breathe g4 |
  fis1 | e2 d | a' \breathe e4 fis | b,2 e |
  d e | e1 \breathe | e2 d4 d | b2( d4 c) |
  a1 \breathe | d4 d e2 | c1 ~ | c \breathe |
  d4( e) e2 | f4( a) e( c) | c2( d) | c1
}
% lyrics for sopranos
\putMusic sop.lyrics \lyricmode {
  Herz -- li -- chen Glück -- wunsch zum Ge -- burts -- tag, zum Ge -- burts -- tag, Ge --
  burts -- tag, al -- les Lie -- be, Lie -- be __ für
  Dich sin -- gen wir, für Dich sin -- gen
  wir, für Dich, Herz -- li -- chen Glück -- 
  wunsch. Al -- les Lie -- be __ 
  zum Ge -- burts -- tag für __ Dich!
}

% alto melody and lyrics ...
\putMusic alt.music \relative c'' {
  a2 a4 g | a a g g | f f2 \breathe fis4 | g2( f!) |
  a2. \breathe f8 g | a2 c4 \breathe d | e d8([ c] d4.) c8 | b4 g a2 \breathe |
  a4 a8([ b]) cis4 a | e2. fis4 | d e( cis) d | b b'2 cis4 |
  d2 a | cis1 \breathe | c!2 d4 d | b2.( c4) |
  a1 \breathe | f2( e) | a \breathe f4 g | a( g) f( a) |
  bes( a) g( e) | f( c') a( f) | g1 | g
}
\putMusic alt.lyrics \lyricmode {
  Herz -- li -- chen Glück -- wunsch zum Ge -- burts -- tag, Ge -- burts -- 
  tag, al -- les Lie -- be für dich sin -- gen wir, für dich
  sin -- gen wir, für dich, für dich sin -- gen wir, sin -- gen
  wir für dich, Herz -- li -- chen Glück --
  wunsch. Lie -- be, al -- les Gu -- te __
  zum Ge -- burts -- tag für Dich!
}

% tenors melody and lyrics ...
\putMusic ten.music \relative c' {
  c2 a4 b | c c b8([ c]) g([ c]) | a2 g4 \breathe fis | b2( a) |
  e2. \breathe f8 f | e1 | e' \breathe | b2 cis4 b |
  a1 ~ | a | r | gis2 cis |
  a cis | a1 \breathe | a2 a4 a | fis'1 |
  e2 \breathe a4 c, | d,2( g) | f \breathe f4 g | a( g) f2 |
  f4( g) c2 | c e,4( a) | g2( f) | e1
}
\putMusic ten.lyrics \lyricmode {
  Herz -- li -- chen Glück -- wunsch zum Ge -- burts -- tag, Ge -- burts -- 
  tag, al -- les Lie -- be, Lie -- be für
  dich __ sin -- gen 
  wir für dich. Herz -- li -- chen Glück --
  wunsch, al -- les Lie -- be, al -- les Gu -- te
  zum Ge -- burts -- tag für __ Dich!
}

% bassos melody and lyrics ...
\putMusic bas.music \relative c' {
  a2 f4 d | e e d e | c2 c4 \breathe c | d2( c) |
  c2. \breathe c8 c | c1 | c' \breathe | e,2 cis |
  d e | cis d | fis4 \breathe cis' a2 | e2 a |
  fis e | e1 \breathe | e2 f4 f | d1 |
  c \breathe | a2( b) | c \breathe c4 e | f( e) c2 |
  d4( c) e( g) | a( f) c( f) | e2( d) | c1
}
\putMusic bas.lyrics \lyricmode {
  Herz -- li -- chen Glück -- wunsch zum Ge -- burts -- tag, Ge -- burts --
  tag, al -- les Lie -- be, Lie -- be,
  für dich sin -- gen wir, für dich sin -- gen
  wir für dich. Herz -- li -- chen Glück --
  wunsch. Lie -- be, al -- les Gu -- te
  zum Ge -- burts -- tag für __ Dich!                
}

% add some tweaks for this sheet of music:
\addEdition sheet % add/activate edition tag "sheet"
% add tweak to StaffGroup-context with current music-folders tag-path
% measure 25 first moment, which is the final moment of this piece:
% allow spanBar, so that the BarLine is not interrupted
\editionMod sheet 25 0/1 StaffGroup.A \once \override BarLine.allow-span-bar = ##t
% lalily defines a shortcut function \spanVisible, which executes this override


% lalilyTest: execute "lalilyCreate" only if parser-output-name matches location-name
% lalilyCreate: 
%   create score with template and options registered for current folder 'jan-peter.alte-hits.choral'
%   template: lalily.vocal.group with default options = staff and lyrics for sop, alt, ten and bas
%   process score to create PDF _and_ MIDI
\lalilyTest

