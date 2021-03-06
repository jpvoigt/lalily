%%%% This file is part of lalily, an extension to lilypond <http://www.lilypond.org/>.
%%%%
%%%% Copyright (C) 2011--2016 Jan-Peter Voigt <jp.voigt@gmx.de>
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

\version "2.19.32"
% include "lalily.ly" from folder above
\include "../lalily.ly"
% include "templates-satb.ly" with template definition and music
\include "02_templates-satb.ly"

% after including the file above, the folder is set,
% but if you include a bunch of files, it may be set to some other path
\setMusicFolder music.choral.altatrinita
% set a different paper
\setPaper \paper {
  indent = 15\mm
}

% the editionEngraver is used with the \lalilyTest and \lalily[Score|Bookpart] commands
% The Score and the Voice context receive it through \layout { ... },
% the Voice context inherits the edition path from the staff editionEngraver.
% Now you can add overrides, sets, breaks and TextScripts to the score,
% without modifying and polluting the music source.

% this file additionally creates a 02_editionEngraver.todo.log, containing all annotations

% activate an edition called "demo"
\addEdition demo

% enter modifications in bar 7 at the first 4th
% for editionEngraver at path #'(music choral altatrinita sop Voice 1)
\editionMod demo 7 0/4 sop.Staff ^\todo #'() #'() "Slur" "Here we modify the shape of the slur"
\editionMod demo 7 0/4 sop.Voice \shape #'((0 . 0)(0 . 1)(0 . 1)(0 . 0)) Slur

\editionMod demo 6 2/4 ten.Staff ^\tweak self-alignment-X #CENTER -\todo #'() #'() "Slur" "Here we modify the shape of the slur"
\editionMod demo 6 2/4 ten.Voice \shape #'((0 . 0)(0 . 1)(0 . 1)(0 . 0)) Slur

\editionMod demo 2 0/4 sop.Staff ^\todo #'() #'() "Lyrics" \markup \column {
  \line { use green color for 'Trinita' }
  \justify { this is set in the Score context, so all Lyrics contexts are affected }
}
\editionMod demo 2 0/4 Score \override LyricText.color = #green
\editionMod demo 3 2/4 Score \revert LyricText.color

\editionMod demo 1 2/4 bas.Lyrics #(make-music 'ExtenderEvent)
\editionMod demo 3 0/4 bas.Lyrics #(make-music 'HyphenEvent)

% accessing parent context so stanza is set in all lyrics inside this StaffGroup
\editionMod demo 9 0/4 bas.Lyrics \set StaffGroup.stanza = "St."

% add a dynamic event
\editionMod demo 9 0/4 sop.Voice \f

% inserting some senseless breaks
\editionMod demo 10 0/4 Score \break
\editionMod demo 17 0/4 Score { \pageBreak \time 4/4 \mark \default }
\editionMod demo 20 0/4 Score \break

% moving a System ... this doesn't make sense that much, but it shows, how it works
\editionMod demo 10 0/4 Score
\overrideProperty Score.NonMusicalPaperColumn.line-break-system-details #'((Y-offset . 100)(X-offset . 6))

% create PDF ... "demo" edition is active
\lalilyTest
% display annotations on new page (in new bookpart)
% \bookpartIf takes an *optional* conditional function,
% to decide wether to add the given bookpart.
% Here we don't provide a conditional function, so the default is used:
% compare outputname with current file name, so this only happens,
% if this file is compiled directly -- like \lalilyTest
\bookpartIf \bookpart {
  \markuplist \annolist
}

