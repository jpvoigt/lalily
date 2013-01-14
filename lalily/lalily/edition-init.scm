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

(use-modules (lalily edition))

(re-export editionEngraver)
(re-export editionMod)

(re-export propset?)
(re-export override?)
(re-export edition-engraver)
(re-export walk-edition-engravers)
(re-export display-mods)
(re-export display-edition)

; activate edition
(define-public addEdition
  (define-music-function (parser location edition)(string-or-symbol?)
    (if (string? edition) (set! edition (string->symbol edition)))
    (if (not (memq edition (editions))) (set-editions! `(,@(editions) ,edition)))
    (make-music 'SequentialMusic 'void #t)
    ))

; deactivate edition
(define-public removeEdition
  (define-music-function (parser location edition)(string-or-symbol?)
    (if (string? edition) (set! edition (string->symbol edition)))
    (set-editions! (delete edition (editions)))
    (make-music 'SequentialMusic 'void #t)
    ))

; create ISMN string with publisher number and title number
(define-public (create-ismn publisher title)
  (let ((ismn-list (string->list "9790"))
        (check 0))
    (set! ismn-list (append ismn-list (string->list publisher)))
    (set! ismn-list (append ismn-list (string->list title)))
    (let ((p 0)(i 0))
      (for-each (lambda (c)(let ((n (- (char->integer c)(char->integer #\0))))
                             (set! i (1+ i))
                             (if (= 0 (modulo i 2))(set! n (* 3 n)))
                             (set! p (+ p n))
                             )) ismn-list)
      (set! check (modulo (- 10 (modulo p 10)) 10) )
      )
    (string-append "M " publisher "-" title "-" (number->string check))
    ))

; annotations

(re-export piece)
(re-export set-piece!)
(re-export category)
(re-export set-category!)
(re-export title)
(re-export set-title!)
(re-export annotation)
(re-export set-annotation!)
(re-export page-ref)
(re-export set-page-ref!)
(re-export measure)
(re-export set-measure!)
(re-export position)
(re-export set-position!)
(re-export moment)
(re-export set-moment!)
(re-export anno-pos)

(re-export make-anno)
(re-export annotation?)
(re-export annotation<?)
(re-export annotations)
(re-export annoCollect)

(define-public text
  (define-music-function (parser location tweaks opts txt)((list? '()) (list? '()) markup?)
    (let ((m (make-music 'TextScriptEvent 'text txt)))
      (for-each (lambda (p)
                  (if (pair? p)
                      (let ((key (car p))
                            (val (cdr p)))
                        (cond ((eq? key 'style)
                               (ly:music-set-property! m 'text (markup #:style val txt)))
                          (else (ly:music-set-property! m key val)))))) opts)
      (if (> (length tweaks) 0) (ly:music-set-property! m 'tweaks tweaks))
      m)))
(define-public todo
  (define-music-function (parser location tweaks opts title txt)((list? '()) (list? '()) markup? markup?)
    (let ((txta (ly:music-function-extract text))
          (defopts `((style . TBD)(annotation . ,(make-anno 'TODO title txt))())))
      (txta parser location tweaks (assoc-set-all! defopts opts) title)
      )))

(define-public anntitle #{ \markup { \override #'(baseline-skip . 0) \left-column { \huge \bold \fromproperty #'anno:piece \hrule \vspace #0.5 } } #})
(define-public annentry #{ \markup {
  \left-column {
    \override #'(hilite-color . (0.95 0.95 0.95)) \hilite \fill-line { \line { \fromproperty #'anno:index - Takt \fromproperty #'anno:position : \fromproperty #'anno:title } \null }
    \justify { \fromproperty #'anno:text }
    \vspace #0.5
  } } #})

(define-markup-list-command (annolist layout props)()
  (let ((mups (list (interpret-markup layout props #{ \markup \fill-line { \huge \bold "Anmerkungen" } #})))
        (pc #f)
        (ac #f)
        (c 0))
    (for-each (lambda (a)
                (set! c (1+ c))
                (if (not (eq? pc (piece a)))
                    (begin
                     (set! pc (piece a))
                     (set! ac #f)
                     (if (markup? pc) (append! mups (list (interpret-markup layout (cons `((anno:piece . ,pc)) props) anntitle))) )))
                (if (not (eq? ac (category a)))
                    (begin
                     (set! ac (category a))
                     (append! mups (list (interpret-markup layout props #{ \markup \fill-line { $(format "~A" ac) \general-align #Y #CENTER \vspace #2 } #})))
                     ))
                (append! mups (list
                               (interpret-markup layout (cons `((anno:index . ,(format "~2,'0d" c))
                                                                (anno:position . ,(anno-pos a))
                                                                (anno:title . ,(title a))
                                                                (anno:text . ,(annotation a))) props) annentry))))
      (annotations #f))
    mups
    ))

