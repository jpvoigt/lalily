;;;; This file is part of lalily, an extension to lilypond <http://www.lilypond.org/>.
;;;;
;;;; Copyright (C) 2011--2012 Jan-Peter Voigt <jp.voigt@gmx.de>
;;;;
;;;; lalily is free software: you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation, either version 3 of the License, or
;;;; (at your option) any later version.
;;;;
;;;; lalily is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with lalily.  If not, see <http://www.gnu.org/licenses/>.

(use-modules (lalily lyrics)(lalily store)(lalily markup))

(re-export nl)

(define-public (define-lyric-markup mup)
  (define-music-function (parser location lyrics) (ly:music?)
    (music-map
     (lambda (m)
       (begin
        (if (equal? (ly:music-property m 'name) 'LyricEvent)
            (let ((syl (ly:music-property m 'text)))
              (ly:music-set-property! m 'text (markup #:override (cons 'lyric:text syl) mup))))
        m))
     lyrics)))
(define-public lyricSize (define-music-function (parser location size lyrics) (number? ly:music?)
                           (let ((lsf (ly:music-function-extract (define-lyric-markup (markup #:fontsize size #:fromproperty 'lyric:text)))))
                             (lsf parser location lyrics))
                           ))
(define-public lyricStyle (define-music-function (parser location style lyrics) (symbol? ly:music?)
                            (let ((lsf (ly:music-function-extract (define-lyric-markup (markup #:style style #:fromproperty 'lyric:text)))))
                              (lsf parser location lyrics))
                            ))
(define-public lyricScale (define-music-function (parser location scale lyrics) (number? ly:music?)
                            (let ((lsf (ly:music-function-extract (define-lyric-markup (markup #:scale scale 1 #:fromproperty 'lyric:text)))))
                              (lsf parser location lyrics))
                            ))

;; add markup to lyric extender
(define-public addExtMup
  (let ((create-stencil-func (lambda (stop x-off mup)
                               (lambda (grob)
                                 (let* ((orig (ly:grob-original grob))
                                        (siblings (if (ly:grob? orig)
                                                      (ly:spanner-broken-into orig) '() ))
                                        (stencil (ly:lyric-extender::print grob)))
                                   (if (= (length siblings) 0)(set! siblings (list grob)))
                                   (let ((i 1)
                                         (my-grob #f))
                                     (map (lambda (g)
                                            (if (eq? g grob) (set! my-grob (= i stop)))
                                            (set! i (1+ i)))
                                       siblings)
                                     (if my-grob
                                         (ly:stencil-combine-at-edge
                                          (ly:lyric-extender::print grob)
                                          Y UP
                                          (grob-interpret-markup grob (markup #:translate (cons x-off 0) (markup #:smaller mup)))
                                          0)
                                         stencil))))
                               )))
    (define-music-function (parser location iter x-off mup mus)(integer? number? markup? ly:music?)
      #{
        \override LyricExtender #'stencil = $(create-stencil-func iter x-off mup)
        $mus
        \revert LyricExtender #'stencil
      #})))

(define-public addLEx
  (define-music-function (parser location xy txt mus)((number-pair? '(0 . -1)) markup? ly:music?)
    (ly:music-set-property! mus 'lyric-extender-applic txt)
    (ly:music-set-property! mus 'lyric-extender-align-x (car xy))
    (ly:music-set-property! mus 'lyric-extender-align-y (cdr xy))
    mus))
