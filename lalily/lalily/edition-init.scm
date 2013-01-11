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

