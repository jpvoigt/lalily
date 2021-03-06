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

(use-modules (lalily persons))

(re-export person-key)
(re-export person-name)
(re-export person-life)
(re-export get-person)

(re-export person-create)
(re-export person?)
(re-export person-db?)
(re-export person-alist-create)

(define-public (get-person-name key)
  (let ((p (get-person (get-person-store) key)))
    (if (person? p)(person-name p) #f)))
(define-public getPersonName
  (define-scheme-function (key)(string-or-symbol?)
    (if (string? key) (set! key (string->symbol key)))
    (let ((p (get-person (get-person-store) key)))
      (if (person? p)(person-name p)
          (begin (ly:input-warning location "unknown person '~A'" key)
            (format "?~A" key))
          ))))
(define-public (get-person-life key)
  (let ((p (get-person (get-person-store) key)))
    (if (person? p)(person-life p) #f)))
(define-public getPersonLife
  (define-scheme-function (key)(string-or-symbol?)
    (if (string? key) (set! key (string->symbol key)))
    (let ((p (get-person (get-person-store) key)))
      (if (person? p)(person-life p)
          (begin (ly:input-warning location "unknown person '~A'" key)
            (format "(*?~A)" key))
          ))))


(re-export get-person-store)
(re-export set-person-store!)
(define-public (display-person-store)(display-persons (get-person-store)))

(define-public registerPerson
  (define-void-function (sym name life)(string-or-symbol? string? string?)
    (if (string? sym) (set! sym (string->symbol sym)))
    (register-person! (get-person-store) sym name life)
    ))


(define-public setComposer #f)
(define-public setPoet #f)
(define-public setArranger #f)
(let* ((mups `((composer . ,(markup #:on-the-fly diff-composer
                              (make-execMarkup-markup (lambda (layout props)
                                                        (let ((composerpre (chain-assoc-get 'header:composerpre props
                                                                             (if (or (chain-assoc-get 'header:arranger props #f) (chain-assoc-get 'header:poet props #f))
                                                                                 (get-registry-val '(lalily person composer pre) "Musik:") "")))
                                                              (composername (chain-assoc-get 'header:composername props #f))
                                                              (composerlife (chain-assoc-get 'header:composerlife props ""))
                                                              (composition (chain-assoc-get 'header:composition props #f)))
                                                          (if (not (markup? composerpre)) (set! composerpre ""))
                                                          (if composername
                                                              (if composition
                                                                  #{ \markup \line { $composerpre \override #'(baseline-skip . 2) \left-column { $composername \smaller \smaller $composerlife } $composition } #}
                                                                  #{ \markup \line { $composerpre $composername \smaller \smaller $composerlife } #})
                                                              ""))))
                              ))
               (arranger . ,(markup
                             (make-execMarkup-markup (lambda (layout props)
                                                       (let ((arrangerpre (chain-assoc-get 'header:arrangerpre props (get-registry-val '(lalily person arranger pre) "Satz:")))
                                                             (arrangername (chain-assoc-get 'header:arrangername props #f))
                                                             (arrangerlife (chain-assoc-get 'header:arrangerlife props ""))
                                                             (arrangement (chain-assoc-get 'header:arrangement props #f)))
                                                         (if arrangername
                                                             (if arrangement #{ \markup \line { $arrangerpre \override #'(baseline-skip . 2) \left-column { $arrangername \smaller \smaller $arrangerlife } $arrangement } #}
                                                                 #{ \markup \line { $arrangerpre $arrangername \smaller \smaller $arrangerlife } #})
                                                             ""))))
                             ))
               (poet . ,(markup
                         (make-execMarkup-markup (lambda (layout props)
                                                   (let ((poetpre (chain-assoc-get 'header:poetpre props (get-registry-val '(lalily person poet pre) "Text:")))
                                                         (poetname (chain-assoc-get 'header:poetname props #f))
                                                         (poetlife (chain-assoc-get 'header:poetlife props ""))
                                                         (poem (chain-assoc-get 'header:poem props #f)))
                                                     (if poetname
                                                         (if poem #{ \markup \line { $poetpre \override #'(baseline-skip . 2) \left-column { $poetname \smaller \smaller $poetlife } $poem } #}
                                                             #{ \markup \line { $poetpre $poetname \smaller \smaller $poetlife } #})
                                                         ""))))
                         ))
               ))
       (set-person! (lambda (piece act key)
                      (cond ((markup? key)(set-default-header piece act key))
                        ((symbol? key)
                         (let ((person (get-person (get-person-store) key))
                               (mup (get-registry-val `(lalily person mup ,act) (assoc-get act mups #f #f))))
                           (if (person? person)
                               (begin
                                (set-default-header piece (string->symbol (format "~Aname" act)) (person-name person))
                                (set-default-header piece (string->symbol (format "~Alife" act)) (person-life person))
                                ) (ly:input-warning (*location*) "unknown person '~A' (~A)" key act))
                           (if (markup? mup)
                               (set-default-header piece act mup))
                           ))
                        ((pair? key)
                         (let ((mup (assoc-get act mups (get-registry-val `(lalily person mup ,act)) #f)))
                           (set-default-header piece (string->symbol (format "~Aname" act)) (car key))
                           (set-default-header piece (string->symbol (format "~Alife" act)) (cdr key))
                           (if (markup? mup)
                               (set-default-header piece act mup))
                           ))

                        (else (ly:input-message location "unknown person-element: (~A) ~A" act key)))
                      ))
       (symorpair? (lambda (v) (or (markup? v)(symbol? v)(and (pair? v)(markup? (car v))(markup? (cdr v)))))))
  (set! setComposer
        (define-void-function (key)(symorpair?)
          (set-person! (get-music-folder) 'composer key)))
  (set! setPoet
        (define-void-function (key)(symorpair?)
          (set-person! (get-music-folder) 'poet key)))
  (set! setArranger
        (define-void-function (key)(symorpair?)
          (set-person! (get-music-folder) 'arranger key)))
  )

(define-markup-command (personName layout props key)(string-or-symbol?)
  (if (string? key) (set! key (string->symbol key)))
  (let ((p (get-person (get-person-store) key)))
    (interpret-markup layout props
      (if (person? p)(person-name p)
          (begin (ly:input-warning location "unknown person '~A'" key)
            (format "?~A" key))
          ))))
(define-markup-command (personLife layout props key)(string-or-symbol?)
  (if (string? key) (set! key (string->symbol key)))
  (let ((p (get-person (get-person-store) key)))
    (interpret-markup layout props
      (if (person? p)(person-life p)
          (begin (ly:input-warning location "unknown person '~A'" key)
            (format "?~A" key))
          ))))
