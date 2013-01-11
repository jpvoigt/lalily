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
(define-public (get-person-life key)
  (let ((p (get-person (get-person-store) key)))
    (if (person? p)(person-life p) #f)))

(re-export get-person-store)
(re-export set-person-store!)
(define-public (display-person-store)(display-persons (get-person-store)))

(define-public registerPerson (define-music-function (parser location sym name life)(symbol? string? string?)
     (register-person! (get-person-store) sym name life)
     (make-music 'SequentialMusic 'void #t)))


(define-public setComposer #f)
(define-public setPoet #f)
(define-public setArranger #f)
(let* ((mups `((composer . ,(markup
                     #:on-the-fly diff-composer (make-execMarkup-markup (lambda (layout props)
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
               (arranger . ,(markup (make-execMarkup-markup (lambda (layout props)
                           (let ((arrangerpre (chain-assoc-get 'header:arrangerpre props (get-registry-val '(lalily person arranger pre) "Satz:")))
                                 (arrangername (chain-assoc-get 'header:arrangername props #f))
                                 (arrangerlife (chain-assoc-get 'header:arrangerlife props ""))
                                 (arrangement (chain-assoc-get 'header:arrangement props #f)))
                                (if arrangername 
                                  (if arrangement #{ \markup \line { $arrangerpre \override #'(baseline-skip . 2) \left-column { $arrangername \smaller \smaller $arrangerlife } $arrangement } #}
                                    #{ \markup \line { $arrangerpre $arrangername \smaller \smaller $arrangerlife } #})
                                  ""))))
               ))
               (poet . ,(markup (make-execMarkup-markup (lambda (layout props)
                           (let ((poetpre (chain-assoc-get 'header:poetpre props (get-registry-val '(lalily person poet pre) "Text:")))
                                 (poetname (chain-assoc-get 'header:poetname props #f))
                                 (poetlife (chain-assoc-get 'header:poetlife props ""))
                                 (poem (chain-assoc-get 'header:poem props #f)))
                                (if poetname 
                                  (if poem #{ \markup \line { Text: \override #'(baseline-skip . 2) \left-column { $poetname \smaller \smaller $poetlife } $poem } #}
                                    #{ \markup \line { Text: $poetname \smaller \smaller $poetlife } #})
                                  ""))))
               ))
       ))
       (set-person! (lambda (parser location piece act key)
                 (cond ((markup? key)(set-default-header parser location piece act key))
                       ((symbol? key)
                        (let ((person (get-person (get-person-store) key))
                              (mup (get-registry-val `(lalily person mup ,act) (assoc-get act mups #f #f))))
                             (set-default-header parser location piece (string->symbol (format "~Aname" act)) (person-name person))
                             (set-default-header parser location piece (string->symbol (format "~Alife" act)) (person-life person))
                             (if (markup? mup)
                                 (set-default-header parser location piece act mup))
                       ))
                       ((pair? key)
                        (let ((mup (assoc-get act mups (get-registry-val `(lalily person mup ,act)) #f)))
                             (set-default-header parser location piece (string->symbol (format "~Aname" act)) (car key))
                             (set-default-header parser location piece (string->symbol (format "~Alife" act)) (cdr key))
                             (if (markup? mup)
                                 (set-default-header parser location piece act mup))
                       ))
                       
                       (else (ly:input-message location "unknown person-element: (~A) ~A" act key)))
       ))
       (symorpair? (lambda (v) (or (markup? v)(symbol? v)(and (pair? v)(string? (car v))(string? (cdr v)))))))
      (set! setComposer (define-music-function (parser location key)(symorpair?)
          (set-person! parser location (get-music-folder) 'composer key)
          (make-music 'SequentialMusic 'void #t)))
      (set! setPoet (define-music-function (parser location key)(symorpair?)
          (set-person! parser location (get-music-folder) 'poet key)
          (make-music 'SequentialMusic 'void #t)))
      (set! setArranger (define-music-function (parser location key)(symorpair?)
          (set-person! parser location (get-music-folder) 'arranger key)
          (make-music 'SequentialMusic 'void #t)))
)
