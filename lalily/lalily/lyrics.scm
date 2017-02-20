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

(define-module (lalily lyrics))
(use-modules (lily)(lalily store)(lalily markup))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Function to extract strings from lyrics.

; \nl command that inserts the placeholder event into a lyrics
(define-public nl (make-music 'LineBreakEvent))

(define (lyrics->list lyrics append-stanza linebreakindicator)
  "Return only syllables and hyphens from  @code{lyrics}."
  (if (ly:music? lyrics)
      (cond
       ((eq? (ly:music-property lyrics 'name) 'LyricEvent)
        (let ((artic (ly:music-property lyrics 'articulations))
              (text (ly:music-property lyrics 'text)))
          (for-each (lambda (art)
                      (cond
                       ((eq? (ly:music-property art 'name) 'HyphenEvent)
                        (set! text `(,text "--")))
                       ) #f) artic)
          text
          ))
       ((eq? (ly:music-property lyrics 'name) 'HyphenEvent)
        (list "--"))
       ((eq? (ly:music-property lyrics 'name) 'LineBreakEvent)
        (list linebreakindicator))
       ((and (eq? (ly:music-property lyrics 'name) 'PropertySet)
             (eq? (ly:music-property lyrics 'symbol) 'stanza))
        (if append-stanza
            (begin (append-stanza (markup #:bold (ly:music-property lyrics 'value))) '())
            (markup #:bold (ly:music-property lyrics 'value))))
       (else (let ((elt (ly:music-property lyrics 'element))
                   (elts (ly:music-property lyrics 'elements)))
               (if (ly:music? elt)
                   (lyrics->list elt append-stanza linebreakindicator)
                   (if (null? elts)
                       '()
                       (map (lambda(x)
                              (lyrics->list x append-stanza linebreakindicator))
                         elts)))))
       )
      '()))

(define (flatten-nonmarkup-list x)
  "Unnest list, but don't flatten markup constructs!"
  (cond ((null? x) '())
    ((not (pair? x)) (list x))
    (else (append (if (markup? (car x))
                      (list (car x))
                      (flatten-nonmarkup-list (car x)))
            (flatten-nonmarkup-list (cdr x))))))

(define (reduce-hyphens text)
  (let eat ((wd (car text)) (wds (cdr text)))
    (cond
     ((null? wds) (list wd))
     ((and (equal? "--" (car wds)) (not (null? (cdr wds))))
      (eat (markup #:concat (wd (cadr wds)))
        (cddr wds)))
     (else (cons (markup wd) (eat (car wds) (cdr wds)))))))

(define (split-on predicate? l)
  (let loop ((h '()) (r l))
    (cond
     ((null? r)
      (if (null? h) h (list (reverse h))))
     ((predicate? (car r))
      (if (null? h)
          (loop h (cdr r))
          (cons (reverse h) (loop '() (cdr r)))))
     (else
      (loop (cons (car r) h) (cdr r))))))

(define-markup-command (verse layout props lyrics) ((lambda (m)(or (list? m)(ly:music? m))))
  "Verse command that marks up a column of \\nl-separated lines"
  (let ((mup (get-markup-producer verse-markup))
        (props (cons (list (cons 'absolute-music-path #t)) props) ))
    (interpret-markup layout props (mup layout props (list lyrics)))))
(define-markup-command (Verse layout props lyrics) ((lambda (m)(or (list? m)(ly:music? m))))
  "Verse command that marks up a column of \\nl-separated lines"
  (let ((mup (get-markup-producer Verse-markup))
        (props (cons (list (cons 'absolute-music-path #f)) props) ))
    (interpret-markup layout props (mup layout props (list lyrics)))))

(let ((vmupp (lambda (layout props mabs arg)
               (let ((linebreakindicator (chain-assoc-get 'linebreakindicator props "/"))
                     (stanzblock (chain-assoc-get 'stanza-block props #t))
                     (display-nl (chain-assoc-get 'display-nl props #f))
                     (make-line (chain-assoc-get 'make-line props make-justify-markup))
                     (stlist (list))
                     (lyrics (if (list? arg)(get-music (create-music-path mabs arg)) arg))
                     )
                 (let*
                  ((append-stanza (lambda (stm)(set! stlist (append stlist (list stm)))))
                   (get-stanza (lambda ()(if (> (length stlist) 0) (car stlist) #f)))
                   (split-cond? (lambda (a) (and
                                             (not display-nl)
                                             (equal? a linebreakindicator))))
                   (list-of-lines (map
                                   (lambda (l) (make-line (reduce-hyphens l)))
                                   (split-on split-cond? (flatten-nonmarkup-list (lyrics->list lyrics (if stanzblock append-stanza #f) linebreakindicator)))))
                   )
                  (if (get-stanza)
                      (markup #:line ((get-stanza) (make-column-markup list-of-lines)))
                      (make-column-markup list-of-lines))
                  ))
               )))
  (register-markup-producer verse-markup (lambda (layout props args) (vmupp layout props #t (car args))))
  (register-markup-producer Verse-markup (lambda (layout props args) (vmupp layout props #f (car args))))
  )
