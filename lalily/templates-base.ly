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
\include "../lalily.ly"
#(ly:set-option 'relative-includes lalily-relincl-tmp)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Generic

\registerTemplate generic
#(define-music-function (piece options)(list? list?)
   (get-music piece))

\registerTemplate NOTFOUND
#(define-music-function (piece options)(list? list?)
   (ly:input-message (*location*) "No template specified for [~A]!" (glue-list piece "."))
   (get-music piece))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% init contexts

\registerTemplate lalily.init.Voice
#(define-music-function (piece options)(list? list?)
   (let* ((localsym (assoc-get 'init-path options '(init) #f))
          (deepsym (assoc-get 'deepsym options 'init #f))
          (deepdef (assoc-get 'deepdef options #{ #}))
          (deepm #{ \getMusicDeep $deepdef #deepsym #}))
     #{
       \getMusic $deepm $localsym
     #}))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% create a group
\registerTemplate lalily.group
#(define-music-function (piece options)(list? list?)
   (let* ((elms (assoc-get 'part options (assoc-get 'element options '())))
          (group (assoc-get 'group options #f))
          (group-mods (assoc-get 'group-mods options #f))
          (remove-tags (assoc-get 'remove-tags options #f))
          (parts (if (> (length elms) 0)
                     (make-music 'SimultaneousMusic 'elements
                       (map
                        (lambda (p)
                          (let* ((opts (cdr p))
                                 (template (assoc-get 'template opts '(generic)))
                                 (path (assoc-get 'music opts (list (car p))))
                                 (part #{ \callTemplate ##t #template #path #opts #})
                                 )
                            (if (and (list? remove-tags)(> (length remove-tags) 0))
                                (removeWithTag remove-tags part)
                                part)
                            )) elms))
                     (make-music 'SimultaneousMusic 'void #t)))
          )
     (if (symbol? group)
         #{
           \new $group \with {
             $(if (ly:context-mod? group-mods) group-mods)
             \consists \editionEngraver $piece
           } $parts
         #}
         parts
         )
     ))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Transpose

\registerTemplate lalily.transpose
#(define-music-function (piece options)(list? list?)
   (let ((template (ly:assoc-get 'template options #f #f))
         (opts (let ((pce (ly:assoc-get 'piece options #f #f))) (if pce (get-default-options pce) options)))
         (pce (ly:assoc-get 'piece options piece #f))
         (pdiff (ly:assoc-get 'transpose options piece #f) )
         (natpit (get-option 'naturalize options #f))
         )
     (define (naturalize-pitch p)
       (let ((o (ly:pitch-octave p))
             (a (* 4 (ly:pitch-alteration p)))
             ;; alteration, a, in quarter tone steps,
             ;; for historical reasons
             (n (ly:pitch-notename p)))
         (cond
          ((and (> a 1) (or (eq? n 6) (eq? n 2)))
           (set! a (- a 2))
           (set! n (+ n 1)))
          ((and (< a -1) (or (eq? n 0) (eq? n 3)))
           (set! a (+ a 2))
           (set! n (- n 1))))
         (cond
          ((> a 2) (set! a (- a 4)) (set! n (+ n 1)))
          ((< a -2) (set! a (+ a 4)) (set! n (- n 1))))
         (if (< n 0) (begin (set! o (- o 1)) (set! n (+ n 7))))
         (if (> n 6) (begin (set! o (+ o 1)) (set! n (- n 7))))
         (ly:make-pitch o n (/ a 4))))
     (define (naturalize music)
       (let ((es (ly:music-property music 'elements))
             (e (ly:music-property music 'element))
             (p (ly:music-property music 'pitch)))
         (if (pair? es)
             (ly:music-set-property!
              music 'elements
              (map (lambda (x) (naturalize x)) es)))
         (if (ly:music? e)
             (ly:music-set-property!
              music 'element
              (naturalize e)))
         (if (ly:pitch? p)
             (begin
              (set! p (naturalize-pitch p))
              (ly:music-set-property! music 'pitch p)))
         music))
     (if (not (list? pce))(set! pce (list pce)))
     (let ((transp
            (ly:music-transpose
             (ly:music-deep-copy
              (call-template template pce options)
              ) pdiff)
            ))
       (if natpit (naturalize transp) transp))
     ))
\parserDefine setTransposedTemplate
#(define-void-function (t1 t2 piece tmpl options)
   (ly:pitch? ly:pitch? list? list? list?)
   (set-default-template piece '(lalily transpose)
     (assoc-set-all! options
       `((transpose . ,(ly:pitch-diff t2 t1))
         (template . ,tmpl)))))

