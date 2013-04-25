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

#(define lalily-relincl-tmp (ly:get-option 'relative-includes))
#(ly:set-option 'relative-includes #t)
\include "../../lalily.ly"
#(ly:set-option 'relative-includes lalily-relincl-tmp)

% these scheme functions are to be used with the shape command
% for example: { \shape \shY #.3 c8( d e) }
% to shape the slur a little bit more concave
#(define (shy-type? v)
   (or (number? v)
       (number-pair? v)
       (and
        (list? v)
        (every (lambda (x) (or (number? x)(number-pair? x))) v)
        )
       ))
\parserDefine shY
#(define-scheme-function (parser location dy)(shy-type?)
   (let ((mod-fun (lambda (m) (cond ((number-pair? m)
                                     (let ((dy (car m))(dz (cdr m)))
                                       `((0 . ,dy)(0 . ,(+ dy dz))(0 . ,(+ dy dz))(0 . ,dy))
                                       ))
                                ((number? m)
                                 `((0 . 0)(0 . ,m)(0 . ,m)(0 . 0))
                                 )
                                (else (ly:input-warning location "type??? ~A ~A ~A" m grob dy) '((0 . 0)(0 . 0)(0 . 0)(0 . 0)))
                                )
                    )))
     (if (list? dy)
         (map (lambda (y) (mod-fun y)) dy)
         (mod-fun dy)
         )
     ))
\parserDefine shy
#(define-music-function (parser location grob dy)(string? shy-type?)
   #{ \shape $grob \shY #dy #})

#(define (nol? v) (or (number? v)(and (list v)(every number? v))))
\parserDefine stretchX
#(define-scheme-function (parser location xf)(nol?)
   (if (list? xf)
       (map (lambda (x)
              (if (> xf 0)
                  `((0 . 0)(,x . 0)(,(* 2 x) . 0)(,(* 3 x) . 0))
                  `((,(* 3 x) . 0)(,(* 2 x) . 0)(,(* 1 x) . 0)(0 . 0))
                  )
              ) xf)
       (if (> xf 0)
           `((0 . 0)(,xf . 0)(,(* 2 xf) . 0)(,(* 3 xf) . 0))
           `((,(* 3 xf) . 0)(,(* 2 xf) . 0)(,(* 1 xf) . 0)(0 . 0))
           )
       ))

#(define (nop? v) (or (number? v)(and (list? v)(every (lambda (y) (or (number? y)(number-pair? y))) v))))
\parserDefine stretchXY
#(define-scheme-function (parser location xf yf)(nol? nop?)
   (if (not (list? xf)) (set! xf (list xf)))
   (if (not (list? yf)) (set! yf (list yf)))
   (map (lambda (x y)
          (let* ((oy (if (pair? y) (car y) 0))
                 (iy (+ oy (if (pair? y) (cdr y) y))))
            (if (> x 0)
                `((0 . ,oy)(,x . ,iy)(,(* 2 x) . ,iy)(,(* 3 x) . ,oy))
                `((,(* 3 x) . ,oy)(,(* 2 x) . ,iy)(,(* 1 x) . ,iy)(0 . ,oy))
                )
            )) xf yf)
   )

% for backward-compatibility
\parserDefine stretch
#(define-music-function (parser location grob xf)(string? nol?)
   (if (lalily:verbose) (ly:input-message location "deprecated stretch"))
   #{ \shape $grob \stretchX #xf #})
\parserDefine stretchPunch
#(define-music-function (parser location grob xf yf)(string? nol? nop?)
   (if (lalily:verbose) (ly:input-message location "deprecated stretchPunch"))
   #{ \shape $grob \stretchXY #xf #yf #})
