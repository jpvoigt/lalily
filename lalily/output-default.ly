%%%% This file is part of lalily, an extension to lilypond <http://www.lilypond.org/>.
%%%%
%%%% Copyright (C) 2011--2012 Jan-Peter Voigt <jp.voigt@gmx.de>
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
%%% this file is included by lalily.ily and won't compile itself

\paper {
  % if default paper is set, insert it in global paper specs
  $(let ((defpap (get-registry-val '(lalily paper default)) ))
  (if (list? defpap) (get-paper defpap) #{ \paper { } #} )
  )
}
\layout {
  % if default layout is set, insert it in global layout specs
  $(let ((defpap (get-registry-val '(lalily layout default)) ))
  (if (list? defpap) (get-layout defpap) #{ \layout { } #} )
  )
}
\midi {
  % if default midi is set, insert it in global midi specs
  $(let ((defmid (get-registry-val '(lalily midi default)) ))
  (if (list? defmid) (get-midi defmid) #{ \midi { } #} )
  )
}


%{
/usr/bin/python: /home/jpv/lily2.17/lilypond/usr/lib/libz.so.1: no
version information available (required by /usr/bin/python) convert-ly
(GNU LilyPond) 2.17.96  convert-ly: »« wird verarbeitet... Anwenden
der Umwandlung: 2.17.0, 2.17.4, 2.17.5, 2.17.6, 2.17.11, 2.17.14,
2.17.15, 2.17.18, 2.17.19, 2.17.20, 2.17.25, 2.17.27, 2.17.29
%}


%{
convert-ly (GNU LilyPond) 2.19.37  convert-ly: Processing `'...
Applying conversion: 2.17.97, 2.18.0, 2.19.2, 2.19.7, 2.19.11,
2.19.16, 2.19.22, 2.19.24, 2.19.28, 2.19.29, 2.19.32
%}
