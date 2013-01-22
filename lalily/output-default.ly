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

\version "2.16.0"
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
