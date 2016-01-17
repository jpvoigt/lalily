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

#(define (sized-mup size abs-size header opts)
   (let* (
           (header-size (assoc-get size opts #f #f))
           (header-abs (assoc-get abs-size opts #f #f))
           (header-mup (markup #:fromproperty header))
           )
     (cond
      ((number? header-size) (markup #:fontsize header-size header-mup))
      ((and (number? header-abs)(> header-abs 0)) (markup #:abs-fontsize header-abs header-mup))
      (else header-mup)
      )
     ))


bookTitle =
#(define-scheme-function (opts)(list?)
   (let ((title (sized-mup 'title-size 'title-abs 'header:title opts))
         (subtitle (sized-mup 'subtitle-size 'subtitle-abs 'header:subtitle opts))
         (subsubtitle (sized-mup 'subsubtitle-size 'subsubtitle-abs 'header:subsubtitle opts))
         )
     #{
       \markup {
         \override #'(baseline-skip . 3.5)
         \column {
           \fill-line { \fromproperty #'header:dedication }
           \override #'(baseline-skip . 3.5)
           \column {
             \fill-line {
               \huge \larger \larger \bold
               $title
             }
             \fill-line {
               \large \bold
               $subtitle
             }
             \fill-line {
               \smaller \bold
               $subsubtitle
             }
             \fill-line {
               \fromproperty #'header:poet
               { \large \bold \fromproperty #'header:instrument }
               \fromproperty #'header:composer
             }
             \fill-line {
               \fromproperty #'header:meter
               \fromproperty #'header:arranger
             }
           }
         }
       }
     #}))


%{
convert-ly (GNU LilyPond) 2.19.36  convert-ly: Processing `'...
Applying conversion: 2.19.2, 2.19.7, 2.19.11, 2.19.16, 2.19.22,
2.19.24, 2.19.28, 2.19.29, 2.19.32
%}
