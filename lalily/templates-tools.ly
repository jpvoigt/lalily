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

#(define lalily-relincl-tmp (ly:get-option 'relative-includes))
#(ly:set-option 'relative-includes #t)
\include "../lalily.ly"
#(ly:set-option 'relative-includes lalily-relincl-tmp)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% some tools

% return list - the parser can return a list, given in dot-notation
\parserDefine Path
#(define-scheme-function (parser location p)(list?) p)

% create a symbol list from string separated by optional character 's' (= '/')
\parserDefine PathS
#(define-scheme-function (parser location s p)((char? #\/) string?)
   (map (lambda (e) (if (> (string-length e) 0) (string->symbol e) '/)) (string-split p s)))

% create a pair from a list (that is the first two elements)
\parserDefine Pair
#(define-scheme-function (parser location p)(list?)
   (cond
    ((>= (length p) 2)
     (if (> (length p) 2)
     (ly:input-warning location "more than 2 elements: ~A" p))
     (cons (car p) (cadr p)))
    ((> (length p) 0) (cons (car p) #f))
    (else '(#f . #f))
    ))
% create a pair from a string
\parserDefine PairS
#(define-scheme-function (parser location s p)((char? #\|) string?)
   (ly:music-function-exec Pair parser location
     (string-split p s)))

% give a template warning
\parserDefine deprecateTemplate
#(define-void-function (parser location)()
   (ly:input-warning location "template [~A] is deprecated!" (glue-list (get-current-template) ".")))


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% templates

% mirror another music-folder
% needs option 'mirror-path
% may set other options fo the inherited templated
\registerTemplate lalily.mirror
#(define-music-function (parser location piece options)(list? list?)
   (let ((path (assoc-get 'mirror-path options #f #f)))
     (if (not (list? path))
         (begin
          (ly:input-warning location "no mirror-path! (~A | ~A)" path piece)
          (set! path '(..))
          ))
     #{
       \createScoreWithOptions #path #options
     #}))

