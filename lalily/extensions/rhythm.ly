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

#(define lalily-relincl-tmp (ly:get-option 'relative-includes))
#(ly:set-option 'relative-includes #t)
\include "../../lalily.ly"
#(ly:set-option 'relative-includes lalily-relincl-tmp)

\parserDefine applyRhythm
#(let ((seqpred? (lambda (m)
                   (and (ly:music? m)
                        (eq? 'SequentialMusic (ly:music-property m 'name))
                        (list? (ly:music-property m 'elements))))))
   (define-music-function (parser location pat mus)(seqpred? seqpred?)
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
       (make-music 'SequentialMusic 'elements (map
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
#(define-scheme-function (parser location proc)(procedure?)
   (lambda (context)
     (let ((last-fraction #f)) ; remember last time-sig-fraction in this context
       `(
         (process-music
          . ,(lambda (trans)
               (let (; get current time-sig-fraction
                     (frac (ly:context-property context 'timeSignatureFraction)))
                 ; compare the current with the last fraction
                 (if (and (not (equal? last-fraction frac))
                          (pair? frac))
                     ; if they are not equal, do something ...
                     (begin
                      ; action for this engraver
                      (proc context trans)
                      ; set last-fraction
                      (set! last-fraction frac)
                      )))))
         )
       )))

