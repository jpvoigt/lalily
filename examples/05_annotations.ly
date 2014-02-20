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

\version "2.17.97"
% include "lalily.ly" from folder above
\include "../lalily.ly"
% include "templates-satb.ly" with template definition and music
\include "02_templates-satb.ly"

\addEdition annotations
\editionMod annotations 2 2/4 alt.Voice.A ^\todo "E?" "Shall this be E?"

% place some music with an annotation over the existing
\putMusic ten.melody <<
  \getMusic ten.melody
  { s1*3 | <>^\todo "X?" "And here?" }
>>

\lalilyTest

\markuplist \annolist

