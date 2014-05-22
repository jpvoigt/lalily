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
\include "../lalily.ly"
#(ly:set-option 'relative-includes lalily-relincl-tmp)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Generic

\registerTemplate generic
#(define-music-function (parser location piece options)(list? list?)
   (get-music piece location))

\registerTemplate NOTFOUND
#(define-music-function (parser location piece options)(list? list?)
   (ly:input-message location "No template specified for [~A]!" (glue-list piece "."))
   (get-music piece location))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% init contexts

\registerTemplate lalily.init.Voice
#(define-music-function (parser location piece options)(list? list?)
   (let* ((localsym (assoc-get 'init-path options '(init) #f))
          (deepsym (assoc-get 'deepsym options 'init #f))
          (deepdef (assoc-get 'deepdef options #{ #}))
          (deepm #{ \getMusicDeep {} #deepsym #}))
     #{
       \getMusic $deepm $localsym
     #}))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Transpose

\registerTemplate lalily.transpose
#(define-music-function (parser location piece options)(list? list?)
   (let ((template (ly:assoc-get 'template options #f #f))
         (opts (let ((pce (ly:assoc-get 'piece options #f #f))) (if pce (get-default-options pce location) options)))
         (pce (ly:assoc-get 'piece options piece #f))
         (pdiff (ly:assoc-get 'transpose options piece #f) )
         )
     (if (not (list? pce))(set! pce (list pce)))
     (ly:music-transpose (ly:music-deep-copy
                          (call-template template parser location pce options)
                          ) pdiff)
     ))
\parserDefine setTransposedTemplate
#(define-void-function (parser location t1 t2 piece tmpl options)
   (ly:pitch? ly:pitch? list? list? list?)
   (set-default-template piece '(lalily transpose)
     (assoc-set-all! options
       `((transpose . ,(ly:pitch-diff t2 t1))
         (template . ,tmpl)))))
