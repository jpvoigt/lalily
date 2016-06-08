;;;; This file is part of lalily, an extension to lilypond <http://www.lilypond.org/>.
;;;;
;;;; Copyright (C) 2011--2016 Jan-Peter Voigt <jp.voigt@gmx.de>
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
(re-export editionModList)

(re-export propset?)
(re-export override?)
(re-export edition-engraver)
(re-export walk-edition-engravers)
(re-export display-mods)
(re-export display-edition)

; activate edition
(re-export addEdition)
; deactivate edition
(re-export removeEdition)


(define-public startModList
  (define-void-function (lid edition-target edition-context-id proc)(symbol? symbol? list? procedure?)
    (optionsInit lid)
    (ly:parser-define! 'current-modlist lid)
    (ly:parser-define! 'current-modproc `((proc . ,proc)
                                          (edition-target . ,edition-target)
                                          (edition-context-id . ,edition-context-id)))
    ))
(define-public addModList
  (let ((frac-or-mom? (@@ (lalily edition) frac-or-mom?)))
    (define-void-function (takt pos mod)
      (integer? frac-or-mom? scheme?)
      (if (and (defined? 'current-modlist)(symbol? current-modlist))
          (optionsAdd current-modlist (list (cons takt pos)) mod)
          (ly:input-warning (*location*) "no modlist started?")
          ))))
(define-public finishModList
  (define-void-function ()()
    (if (and (defined? 'current-modlist)
             (symbol? current-modlist)
             (defined? 'current-modproc)
             (list? current-modproc)
             (symbol? (assoc-get 'edition-target current-modproc))
             (list? (assoc-get 'edition-context-id current-modproc))
             (procedure? (assoc-get 'proc current-modproc))
             )
        (let ((lst (ly:parser-lookup current-modlist))
              (edition-target (assoc-get 'edition-target current-modproc))
              (edition-id (assoc-get 'edition-context-id current-modproc))
              (proc (assoc-get 'proc current-modproc)))
          (for-each (lambda (p)
                      (if (pair? p)
                          (let ((mod (proc (cdr p))))
                            (editionMod edition-target (caar p) (cdar p) edition-id mod)
                            )))
            lst))
        (ly:input-warning (*location*) "no modlist started?")
        )))




; create ISMN string with publisher number and title number
(re-export create-ismn)

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

(re-export text)
(re-export todo)

(define-public anntitle #{ \markup { \override #'(baseline-skip . 0) \left-column { \huge \bold \fromproperty #'anno:piece \hrule \vspace #0.5 } } #})
(define-public annentry #{ \markup {
  \left-column {
    \override #'(hilite-color . (0.95 0.95 0.95)) \hilite \fill-line { \line { \fromproperty #'anno:index - Takt \fromproperty #'anno:position : \fromproperty #'anno:title } \null }
    \justify { \fromproperty #'anno:text }
    \vspace #0.5
  }
  } #})


(define-markup-list-command (annolist layout props)()
  (let* ((header (ly:assoc-get 'paper:annoTitle props #{ \markup \column { \fill-line { \huge \bold "Annotations" } \vspace#0.3 } #} #f))
         (mups (list (interpret-markup layout props header)))
         (path (ly:chain-assoc-get 'anno-filter props #f #f))
         (pc #f)
         (ac #f)
         (c 0))
    (for-each (lambda (a)
                (set! c (1+ c))
                (if (not (equal? pc (piece a)))
                    (begin
                     (set! pc (piece a))
                     (set! ac #f)
                     (if (markup? pc) (append! mups (list (interpret-markup layout (cons `((anno:piece . ,pc)) props) anntitle))) )))
                (if (not (equal? ac (category a)))
                    (begin
                     (set! ac (category a))
                     (append! mups (list (interpret-markup layout props #{ \markup \fill-line { $(format "~A" ac) \general-align #Y #CENTER \vspace #2 } #})))
                     ))
                (append! mups (list
                               (interpret-markup layout (cons `((anno:index . ,(format "~2,'0d" c))
                                                                (anno:position . ,(anno-pos a))
                                                                (anno:title . ,(title a))
                                                                (anno:text . ,(annotation a))) props) annentry))))
      (annotations path))
    mups
    ))

