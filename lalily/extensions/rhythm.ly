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

#(define lalily-relincl-tmp (ly:get-option 'relative-includes))
#(ly:set-option 'relative-includes #t)
\include "../../lalily.ly"
#(ly:set-option 'relative-includes lalily-relincl-tmp)

\parserDefine applyRhythm
#(let ((seqpred? (lambda (m)
                   (and (ly:music? m)
                        (eq? 'SequentialMusic (ly:music-property m 'name))
                        (list? (ly:music-property m 'elements))))))
   (define-music-function (pat mus)(seqpred? seqpred?)
     (let ((pl '()))
       (for-each
        (lambda (m) (cond
                     ((eq? 'EventChord (ly:music-property m 'name))
                      (set! pl `(,@pl
                                  ,(map (lambda (n) (if (ly:pitch? (ly:music-property n 'pitch))
                                                        (ly:music-property n 'pitch) n))
                                     (ly:music-property m 'elements) ))))
                     ((eq? 'NoteEvent (ly:music-property m 'name))
                      (set! pl `(,@pl (,(ly:music-property m 'pitch)
                                        ,@(ly:music-property m 'articulations)))))
                     )) (ly:music-property mus 'elements))
       (make-music 'SequentialMusic 'elements
         (map
          (lambda (m)
            (let ((ret m))
              (if (eq? 'NoteEvent (ly:music-property m 'name))
                  (set! ret
                        (make-music 'EventChord
                          'elements
                          (let ((nl (car pl)))
                            (set! pl (cdr pl))
                            `(,@(map (lambda (p)
                                       (if (ly:pitch? p)
                                           (make-music 'NoteEvent 'duration (ly:music-property m 'duration) 'pitch p)
                                           p)
                                       )
                                  nl)
                               ,@(ly:music-property m 'articulations)))
                          )))
              ret)
            ) (ly:music-property pat 'elements)))
       )))

% engraver builder
\parserDefine timeSigChangeEngraver
#(define-scheme-function (proc)(procedure?)
   (lambda (context)
     (let ((last-fraction #f)) ; remember last time-sig-fraction in this context
       `(
         (process-music
          . ,(lambda (trans)
               (let (; get current time-sig-fraction
                     (frac (ly:context-property context 'timeSignatureFraction)))
                 ; compare the current with the last fraction
                 (if (and (pair? frac)(not (equal? last-fraction frac)))
                     ; if they are not equal, do something ...
                     (begin
                      ; action for this engraver
                      (proc context trans)
                      ; set last-fraction
                      (set! last-fraction frac)
                      )))))
         )
       )))



%{
/usr/bin/python: /home/jpv/lily2.17/lilypond/usr/lib/libz.so.1: no
version information available (required by /usr/bin/python) convert-ly
(GNU LilyPond) 2.17.96  convert-ly: »« wird verarbeitet... Anwenden
der Umwandlung: 2.17.0, 2.17.4, 2.17.5, 2.17.6, 2.17.11, 2.17.14,
2.17.15, 2.17.18, 2.17.19, 2.17.20, 2.17.25, 2.17.27, 2.17.29
%}


%{
convert-ly (GNU LilyPond) 2.19.36  convert-ly: Processing `'...
Applying conversion: 2.17.97, 2.18.0, 2.19.2, 2.19.7, 2.19.11,
2.19.16, 2.19.22, 2.19.24, 2.19.28, 2.19.29, 2.19.32
%}
