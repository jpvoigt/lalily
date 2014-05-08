%%%% This file is part of lalily, an extension to lilypond <http://www.lilypond.org/>.
%%%%
%%%% Copyright (C) 2011--2014 Jan-Peter Voigt <jp.voigt@gmx.de>
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
% include music, but do not engrave it! The included file uses \lalilyTest, which does not engrave, when the file is included
\include "06_use-case-SATB.ly"

% we want to use options for the template, so clear the opts variable
% this is equivalent to
% opts = #'()
% but can also be written inside curly braces
\optionsInit opts
% append list to #'((mirror-path . (..)))
% the template can get this option via (assoc-get 'mirror-path opts #f #f)
\optionsAdd opts mirror-path #'(..) % set the mirror-path to '..' = upper folder
% now we override the staffs option for the inherited template lalily.vocal.group,
% so we will define all needed sub options:
% set staff-context-modifications
\optionsAdd opts staffs.sop.staff-mods \with {
  instrumentName = "Sopranos"
  shortInstrumentName = "S"
  midiInstrument = "voice oohs"
}
\optionsAdd opts staffs.alt.staff-mods \with {
  instrumentName = "Altos"
  shortInstrumentName = "A"
  midiInstrument = "voice oohs"
}
\optionsAdd opts staffs.ten.staff-mods \with {
  instrumentName = "Tenoros"
  shortInstrumentName = "T"
  midiInstrument = "choir aahs"
}
% set clef for tenor
\optionsAdd opts staffs.ten.clef "G_8"
\optionsAdd opts staffs.bas.staff-mods \with {
  instrumentName = "Bassos"
  shortInstrumentName = "B"
  midiInstrument = "choir aahs"
}
\optionsAdd opts staffs.bas.clef "bass"

% \setTransposedTemplate (like \setDefaultTemplate does with 1-3) combines four commands:
% 1. set music-folder
% 2. set template
% 3. set options
% 4. wrap music in transposition template
% the include sets the current music-folder, so we can create a new one with \musicPath <relative path>
\setTransposedTemplate c' a \musicPath transposed-A lalily.mirror #opts
% we are now in another music-folder, so we inherit all headers from the mirror-path
\inheritAllHeaders \getatree opts mirror-path

% sheet tag for the edition-egraver is still active
% current music-folder is now jan-peter.alte-hits.choral.transposed-A
% but the edition-engraver tags are jan-peter.alte-hits.choral.<...>, so we have to use one folder up
\editionMod sheet 2 2/4 LY_UP.sop.Voice.A \shy #'(-.2 . -.1) Slur
\editionMod sheet 3 0/4 LY_UP.sop.Voice.A \shy #'(.2 . .1) Slur
\editionMod sheet 5 0/4 LY_UP.sop.Voice.A \shy #'(.2 . .1) Slur
\editionMod sheet 16 0/4 LY_UP.sop.Voice.A \shy #.4 Slur
\editionMod sheet 19 0/4 LY_UP.sop.Voice.A \shy #'(-.2 . -.1) Tie
\editionMod sheet 21 0/4 LY_UP.sop.Voice.A \shy #'(.2 . .1) Slur
% I use this little shy helper, that produces shape-modification lists, because I am used to it ...
% you may now better use shapeII by Janek Warchol, which is much more advanced - I am going to change that!

% you can also add annotations:
\editionMod sheet 15 0/4 LY_UP.StaffGroup.A {
  \once \override TextScript.extra-offset = #'(-2 . -3)
  \once \override TextScript.font-shape = #'italic
  <>1*0^"Don't be too sad, that you are one year older now!"
}

% the default standard for lalily.vocal, which search for "global-voice", is { \dynamicUp \autoBeamOff }
\putMusic LY_UP.global-voice { \dynamicUp }

% use another paper - this will not affect the original file
\setPaper \paper {
  indent = 18\mm
  two-sided = ##f
  left-margin = 12\mm
  right-margin = 10\mm
  top-margin = 10\mm
  bottom-margin = 10\mm
}

% create PDF and Midi for this file (conditionally)
\lalilyTest
