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

\version "2.17.29"

#(define lalily-relincl-tmp (ly:get-option 'relative-includes))
#(ly:set-option 'relative-includes #t)
\include "../../lalily.ly"
#(ly:set-option 'relative-includes lalily-relincl-tmp)

\parserDefine emptyPage
#(define-scheme-function (parser location)()
   #{
     \bookpart { \paper { $(get-paper '(lalily empty-head-foot)) } \markup \null }
   #})

\parserDefine createCopyrightMarkup
#(define-scheme-function (parser location options)(list?)
   #{
     \markup {
       \column {
         \fill-line {
           \jpv-cond-override #(lambda (layout props)(if (jpv:print-version layout) 0 2.5)) #'baseline-skip
           \left-column {
             \line { \copyright \on-the-fly #has-rightinfo { ", " \fromproperty #'header:rightinfo } }
             {
               \teeny \sans \line {
                 "Vervielfältigungen jeglicher Art sind gesetzlich verboten / Any unauthorized reproduction is prohibited by law"
             } }
             \versionMarkup
           }
           \jpv-cond-override #(lambda (layout props)(if (jpv:print-version layout) 0 2.5)) #'baseline-skip
           \typewriter \right-column {
             \on-the-fly #has-catname \concat { \fromproperty #'header:catname "-" \cat-number }
             \teeny \on-the-fly #has-ismn \concat { "ISMN " \fromproperty #'header:ismn }
           }
         }
       }
     }
   #})


%{
/usr/bin/python: /home/jpv/lily2.17/lilypond/usr/lib/libz.so.1: no
version information available (required by /usr/bin/python) convert-ly
(GNU LilyPond) 2.17.96  convert-ly: »« wird verarbeitet... Anwenden
der Umwandlung: 2.17.0, 2.17.4, 2.17.5, 2.17.6, 2.17.11, 2.17.14,
2.17.15, 2.17.18, 2.17.19, 2.17.20, 2.17.25, 2.17.27, 2.17.29
%}
