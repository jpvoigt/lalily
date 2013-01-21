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

\version "2.16.0"
% include "lalily.ly" from folder above
\include "../lalily.ly"

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% this file shows, how to define and register a template, how to enter music,
% fitting this template and how to create a PDF of this.
% Compiling this file creates templates-satb.log and templates-satb.edition.log
% beside templates-satb.pdf and templates-satb.midi.
% - in the *.log file you can look for current settings and where music is stored
% - in the *.edition.log file you can look for paths, where an editionEngraver listens
%   (this part is showed in another example file)


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% first we define a template here ...
% this shall reside in a template file either in lalily's folder or in the current folder
% and not in the actual music-files - this is for tutorial/demonstration purposes!
% Music is entered later in this file

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% create and register a template for a choral staff with lyrics
\registerTemplate #'(lalily demo choral staff)
% a template in lalily sontext is a music function with two parameters of kind [list?]
#(define-music-function (parser location piece options)(list? list?)
   ; piece contains the current music folder
   ; options contain options passed to this template
   (let* ((instrname (ly:assoc-get 'instrname options "Voc?" #f))
          (shortname (ly:assoc-get 'shortname options #f #f))
          ; voicename is instrument name, if not specified
          (voicename (ly:assoc-get 'vocname options instrname #f))
          ; staffname is voicename, if not specified
          (staffname (ly:assoc-get 'staffname options voicename #f))
          (lyricname (ly:assoc-get 'lyricname options staffname #f))
          (clef (ly:assoc-get 'clef options "G" #f)))
     #{
       % simultanious music with a Staff and a Lyrics context
       <<
         % create Staff with name 'staffname'
         \new Staff = $staffname \with {
           % create a staff with an editionEngraver tagged with path of current piece
           \consists \editionEngraver $piece
           % set instrument name from options
           instrumentName = $instrname
           shortInstrumentName = $shortname
         } <<
           % create Voice with name 'voicename'
           \new Voice = $voicename {
             % call template #'(global voice) - not relative to this template
             \callTemplate #'(/ global voice) #'() #'()
             % set clef from options
             \clef $clef
             % fetch melody from the current music folder/directory
             \getMusic #'(melody)
           }
           % search for "meta-track" in the music directory tree
           \getMusicDeep #'meta
         >>
         % create Lyrics context with name 'lyricname'
         \new Lyrics = $lyricname \with {
           % use editionEngraver
           \consists \editionEngraver $piece
         }
         % assign Lyrics to Voice $voicename and get music/lyrics from path lyrics in the current directory
         \lyricsto $voicename \lyricmode { \getMusic #'(lyrics) }
       >>
     #}))

% create a template for a SATB StaffGroup
\registerTemplate #'(lalily demo choral satb)
#(define-music-function (parser location piece options)(list? list?)
   #{
     \new StaffGroup \with {
       % disable SpanBar like in ChoirStaff, but leave the possibility to display it with
       % \once \override StaffGroup.BarLine #'allow-span-bar = ##t
       \override BarLine #'allow-span-bar = ##f
     } <<
       % call template #'(choral staff) relative to this template path
       % with path #'(sop) relative to the current piece/path
       % tith options specifying clef, instrument name and short name
       \callTemplate #'(.. staff) #'(sop) #'((clef . "G")(instrname . "Soprano")(shortname . "S"))
       % do the same for alt, ten and bas
       \callTemplate #'(.. staff) #'(alt) #'((clef . "G")(instrname . "Alto")(shortname . "A"))
       \callTemplate #'(.. staff) #'(ten) #'((clef . "G_8")(instrname . "Tenoro")(shortname . "T"))
       \callTemplate #'(.. staff) #'(bas) #'((clef . "bass")(instrname . "Basso")(shortname . "B"))
     >>
   #})

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% second we enter the music and assign it the formaly defined template.
% As mentioned above, the template is defined/registered here for tutorial purposes.
% In production, one would use template files in central places

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% set current music folder and assign a template with options to it
\setDefaultTemplate #'(music choral altatrinita) #'(lalily demo choral satb) #'()
% set title for current (formerly set) music folder
\setTitle "Alta Trinita Beata"
% set composer for current music folder with #'(name . life)
\setComposer #'("Italy" . "15th Cent.")

% "meta-track" for this piece of music added to each staff!
% stored in #'(music choral altatrinita meta)
\putMusic #'(meta) {
  \key f \major \time 2/2
  \repeat volta 2 { s1*16 \spanVisible }
  \repeat volta 2 { s1*8 \spanVisible }
}

% soprano lyrics
% stored in #'(music choral altatrinita sop lyrics)
\putMusic #'(sop lyrics) \lyricmode {
  Al -- ta Tri -- ni -- ta be -- a -- ta,
  da noi sem -- pre ad -- o -- ra -- ta,
  Tri -- ni -- ta glo -- ri -- o -- sa
  u -- ni -- ta ma -- ra vi -- glio -- sa,
  Tu -- sei man -- na sa -- po -- ro -- sa
  e tut -- ta de -- si -- de -- ro -- sa.
}
% soprano music
% stored in #'(music choral altatrinita sop melody)
\putMusic #'(sop melody) \relative c' {
  f2 f4( g) | a2 g4( f) | bes2 a4( g) | a2 a |
  g2 a4( bes) | a2 g4( f) | g( bes) a( g) | f2 f \breathe |
  c'2 c4( d) | bes1 | c4( bes) a( g) | a2 a |
  g4( f) bes( g) | f2 bes4( a) | g( f) bes( g) | f2 f |
  c'2 c4( d) | bes2 c4( d) | ees( d) c( a) | bes2 g |
  f2 g4( a) | bes2 a | g4( f) bes( g) | f2 f |
}
% alto lyrics - reusing soprano lyrics
\putMusic #'(alt lyrics) \getMusic #'(sop lyrics)
% alto music
\putMusic #'(alt melody) \relative c' {
  c2 c4( e) | f2 e4( f) | f2 f4( e) | f2 f |
  e2 f | f e4( f) | e( f) f( e) | c2 c \breathe |
  a'2 a | g1 | g2 f4( e) | f2 f |
  e4( d) d( e) | f2 f | e4( d) d( e) | c2 c |
  a'2 a4( f) | g2 g4( bes) | c( bes) g( f) | f2 e |
  c2 e4( f) | f2 f | e4( d) d( e) | c2 c |
}
% tenoro lyrics - reusing soprano lyrics
\putMusic #'(ten lyrics) \getMusic #'(sop lyrics)
% tenor music
\putMusic #'(ten melody) \relative c' {
  a2 a4( c) | c2 c | d c | c c |
  c2 c4( d) | c2 c4( a) | c( d) c2 | a a \breathe |
  f'2 f | d1 | e2 c | c c |
  c4( a) bes( c) | c2 d4( c) | c( a) bes( c) | a2 a |
  f'2 f | d ees4( f) | g( f) ees( c) | d2 c |
  a2 c | d c | c4( a) bes( c) | a2 a |
}
% basso lyrics - reusing soprano lyrics
\putMusic #'(bas lyrics) \getMusic #'(sop lyrics)
% basso music
\putMusic #'(bas melody) \relative c {
  f2 f4( c) | f2 c4( f) | bes,2 f'4( c) | f2 f | 
  c2 f4( bes,) | f'2 c4( d) | c( bes) f'( c) | f,2 f \breathe |
  f'2 f4( d) | g1 | c,2 f4( c) | f2 f |
  c4( d) g,( c) | f2 bes,4( f') | c( d) g,( c) | f2 f |
  f2 f4( d) | g2 ees4( d) | c( d) ees( f) | bes,2 c |
  f2 c4( f) | bes,2 f' | c4( d) g,( c) | f,2 f |
}

% instantiate this piece of music ... creates PDF, MIDI and a log file
% this only happens, if this file is compiled directly and not included!
% To check, if this is the case, the current outputname is compared to the current file name.
% If the outputname is changed, this may not work!
\lalilyTest

