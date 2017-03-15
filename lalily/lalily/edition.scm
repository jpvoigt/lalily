;;;; -*- master: ../../examples/03_editionEngraver.ly;
;;;; This file is part of lalily, an extension to lilypond <http://www.lilypond.org/>.
;;;;
;;;; Copyright (C) 2011--2015 Jan-Peter Voigt <jp.voigt@gmx.de>
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

(define-module (lalily edition))

(use-modules
 (oop goops)
 (lily)
 (lalily definitions)
 (lalily lascm)
 (lalily laly)
 (lalily store)
 (lalily markup)
 )

(define-public (moment->string mom)
  (if (ly:moment? mom)
      (let ((num (ly:moment-main-numerator mom))
            (den (ly:moment-main-denominator mom))
            (gnum (ly:moment-grace-numerator mom))
            (gden (ly:moment-grace-denominator mom)))
        (format "(~A/~A~A)" num den
          (cond
           ((> gnum 0)(format "+~A/~A" gnum gden))
           ((< gnum 0)(format "~A/~A" gnum gden))
           (else "")
           ))
        )
      "(?:?)"
      ))


;%%%%%%%%%%%%%

(define-class <propset> ()
  (once #:init-value #t #:accessor is-once #:setter set-once! #:init-keyword #:once)
  (symbol #:accessor get-symbol #:setter set-symbol! #:init-keyword #:symbol)
  (value #:accessor get-value #:setter set-value! #:init-keyword #:value)
  (previous #:accessor get-previous #:setter set-previous! #:init-value #f)
  (context #:accessor get-context #:setter set-context! #:init-keyword #:context)
  )
(define-method (do-propset context (prop <propset>))
  (if (get-context prop)
      (let ((parctx (ly:context-find context (get-context prop))))
        (if (ly:context? parctx) (set! context parctx))))
  (set-previous! prop (ly:context-property context (get-symbol prop)))
  (ly:context-set-property! context (get-symbol prop) (get-value prop))
  )
(export do-propset)
(define-method (reset-prop context (prop <propset>))
  (if (get-context prop)
      (let ((parctx (ly:context-find context (get-context prop))))
        (if (ly:context? parctx) (set! context parctx))))
  (ly:context-set-property! context (get-symbol prop) (get-previous prop))
  )
(export reset-prop)

(define-public (propset? p)(is-a? p <propset>))
(define-method (propset->string (ps <propset>))
  (format "~A\\set ~A = ~A" (if (is-once ps) "once " "") (string-append (if (get-context ps) (format "~A." (get-context ps)) "") (format "~A" (get-symbol ps))) (get-value ps)))
(export propset->string)
(define-method (display (o <propset>) port) (display (propset->string o) port))

;%%%%%%%%%%%%%

(define-class <apply-context> ()
  (proc #:accessor procedure #:setter set-procedure! #:init-keyword #:proc)
  )
(define-method (do-apply ctx (a <apply-context>))
  ((procedure a) ctx))
(export do-apply)
(define-public (apply-context? a)(is-a? a <apply-context>))


(define-class <override> ()
  (once #:init-value #t #:accessor is-once #:setter set-once! #:init-keyword #:once)
  (revert #:init-value #f #:accessor is-revert #:setter set-revert! #:init-keyword #:revert)
  (grob #:accessor get-grob #:setter set-grob! #:init-keyword #:grob)
  (prop #:accessor get-prop #:setter set-prop! #:init-keyword #:prop)
  (value #:accessor get-value #:setter set-value! #:init-keyword #:value)
  (context #:accessor get-context #:setter set-context! #:init-keyword #:context)
  )
(define-method (oop->string (o <override>))
  (let* ((ctxn (get-context o))
         (ctxp (if ctxn (format "~A." ctxn) "")))
    (if (is-revert o)
        (string-append "\\revert " ctxp (format "~A " (get-grob o)) (format "#'~A" (get-prop o)))
        (string-append (if (is-once o) "\\once " "") "\\override " ctxp (format "~A " (get-grob o)) (format "#'~A" (get-prop o)) " = " (format "~A" (get-value o)))
        )))
(export oop->string)
(define-method (display (o <override>) port) (display (oop->string o) port))
(define-public (override? o)(is-a? o <override>))

(define-method (do-override ctx (mod <override>))
  (if (get-context mod)
      (let ((parctx (ly:context-find ctx (get-context mod))))
        (if (ly:context? parctx) (set! ctx parctx))))
  (ly:context-pushpop-property ctx (get-grob mod) (get-prop mod) (get-value mod)))
(export do-override)
(define-method (do-revert ctx (mod <override>))
  (if (get-context mod)
      (let ((parctx (ly:context-find ctx (get-context mod))))
        (if (ly:context? parctx) (set! ctx parctx))))
  (ly:context-pushpop-property ctx (get-grob mod) (get-prop mod)))
(export do-revert)

;%%%%%%%%%%%%%

(define-public (editions) #f)
(define-public (set-editions! ed) #f)
(define-public (add-edmod edition takt pos path mod) #f)
(define-public (edition-engraver tag-path) #f)
(define-public (walk-edition-engravers proc) #f)
(define-public (display-mods) #f)
(define-public (display-edition) #f)

(define-public (context-find-edition-engraver context) #f)

(define lalily:edition-tags 'lalily:edition-tags)
(let ((mod-tree (tree-create 'mods))
      (edition-list '())
      (edition-tree (tree-create 'edition))
      (context-count (tree-create 'context)))
  (define (o->sym o) (cond ((symbol? o) o) ((string? o) (string->symbol o)) (else (string->symbol (format "~A" o)))))
  (set! editions (lambda () (if (list? edition-list) edition-list '())))
  (set! set-editions! (lambda (eds) (if (list? eds) (set! edition-list eds) (ly:error "list expected: ~A" eds))))
  (set! add-edmod
        (lambda (edition takt pos path modm)
          (let* ((edition (if (string? edition) (string->symbol edition) edition))
                 (path `(,@path ,takt ,pos ,edition))
                 (mods (tree-get mod-tree path)))
            (if (not (list? mods)) (set! mods '()))
            (cond
             ((ly:music? modm)
              (let ((x 0))
                (define (add-mods modmus ctx)
                  (for-some-music
                   (lambda (m)
                     (cond
                      ((eq? 'ContextSpeccedMusic (ly:music-property m 'name))
                       (let* ((ct (ly:music-property m 'context-type))
                              (elm (ly:music-property m 'element)))
                         (if (eq? 'Bottom ct)
                             #f
                             (begin
                              (add-mods elm ct)
                              #t)
                             )
                         ))
                      ((eq? 'OverrideProperty (ly:music-property m 'name))
                       (let* ((once (ly:music-property m 'once #f))
                              (grob (ly:music-property m 'symbol))
                              (prop (ly:music-property m 'grob-property))
                              (prop (if (symbol? prop)
                                        prop
                                        (car (ly:music-property m 'grob-property-path))))
                              (value (ly:music-property m 'grob-value))
                              (mod (make <override> #:once once #:grob grob #:prop prop #:value value #:context ctx)))
                         (set! mods `(,@mods ,mod))
                         #t
                         ))
                      ((eq? 'RevertProperty (ly:music-property m 'name))
                       (let* ((grob (ly:music-property m 'symbol))
                              (prop (ly:music-property m 'grob-property))
                              (prop (if (symbol? prop)
                                        prop
                                        (car (ly:music-property m 'grob-property-path))))
                              (mod (make <override> #:once #f #:revert #t #:grob grob #:prop prop #:value #f #:context ctx)))
                         (set! mods `(,@mods ,mod))
                         #t
                         ))
                      ((eq? 'PropertySet (ly:music-property m 'name))
                       (let* ((once (ly:music-property m 'once #f))
                              (symbol (ly:music-property m 'symbol))
                              (value (ly:music-property m 'value))
                              (mod (make <propset> #:once once #:symbol symbol #:value value #:context ctx)))
                         (set! mods `(,@mods ,mod))
                         #t
                         ))
                      ((eq? 'ApplyContext (ly:music-property m 'name))
                       (let* ((proc (ly:music-property m 'procedure))
                              (mod (make <apply-context> #:proc proc)))
                         (set! mods `(,@mods ,mod))
                         #t
                         ))
                      ((or
                        (eq? 'OttavaMusic (ly:music-property m 'name))
                        )
                       (set! mods `(,@mods ,(context-mod-from-music m)))
                       #t
                       )
                      ((or
                        (eq? 'TextScriptEvent (ly:music-property m 'name))
                        (eq? 'LineBreakEvent (ly:music-property m 'name))
                        (eq? 'PageBreakEvent (ly:music-property m 'name))
                        (eq? 'PageTurnEvent (ly:music-property m 'name))
                        (eq? 'ApplyOutputEvent (ly:music-property m 'name))
                        (eq? 'MarkEvent (ly:music-property m 'name))
                        (eq? 'KeyChangeEvent (ly:music-property m 'name))

                        (eq? 'PartCombineForceEvent (ly:music-property m 'name))
                        (eq? 'ExtenderEvent (ly:music-property m 'name))
                        (eq? 'HyphenEvent (ly:music-property m 'name))
                        )
                       (set! mods `(,@mods ,m))
                       #t
                       )
                      (else #f)
                      )
                     )
                   modmus))
                (add-mods modm #f)))
             ((ly:context-mod? modm)(set! mods `(,@mods ,modm)))
             )
            (tree-set! mod-tree path mods)
            #f
            )))
  (set! edition-engraver
        (lambda (tag-path . props)
          (let ((eng #f)
                (cmf (if (eq? #t tag-path) (get-music-folder)))) ; current music folder
            (define (get-sym c)(string->symbol (base26 c)))
            (set! eng
                  (lambda (context)
                    (let* ((tag-path tag-path)
                           (tag '())
                           (barnum 0)
                           (measurepos (ly:make-moment 0 1))
                           (ctxid (ly:context-id context))
                           (ctxname (ly:context-name context))
                           (context-mods #f)

                           ; TODO get-paths -> collect from all paths
                           (get-paths
                            (lambda (edition takt pos)
                              (if (and (string? ctxid)(> (string-length ctxid) 0))
                                  (let ((ctxid (string->symbol ctxid)))
                                    `(
                                       (,@tag-path ,ctxname ,ctxid ,takt ,pos ,edition)
                                       (,@tag-path ,ctxid ,takt ,pos ,edition)
                                       (,@tag-path ,ctxname ,takt ,pos ,edition)
                                       (,@tag-path ,takt ,pos ,edition)
                                       (,@tag ,takt ,pos ,edition)
                                       ))
                                  `(
                                     (,@tag-path ,ctxname ,takt ,pos ,edition)
                                     (,@tag-path ,takt ,pos ,edition)
                                     (,@tag ,takt ,pos ,edition)
                                     )
                                  )))
                           (get-mods
                            (lambda ()
                              (let* (;(moment (ly:context-current-moment context))
                                      (measure (ly:context-property context 'currentBarNumber))
                                      (measurePos (ly:context-property context 'measurePosition))
                                      (current-mods (tree-get context-mods (list measure measurePos))))
                                (if (list? current-mods) current-mods '())
                                )))
                           (initialize
                            (lambda (trans)
                              (if (procedure? tag-path) (set! tag-path (tag-path)))

                              (if (not (list? tag-path))

                                  ; TODO edition-id
                                  (let ((edition-id (ly:context-property context 'edition-id #f))
                                        (parent (ly:context-parent context))
                                        (peng #f))

                                    (define (search-peng path eng)
                                      (if (eqv? (object-property eng 'context) parent)
                                          (set! peng eng)))

                                    ;(ly:message "edition-id: ~A" edition-id)
                                    (if (and (not (list? tag-path)) (list? edition-id) (> (length edition-id) 0))
                                        (set! tag-path edition-id)
                                        (begin
                                         (if (ly:context? parent) (walk-edition-engravers search-peng))
                                         (if peng (set! tag-path (object-property peng 'tag-path)))
                                         (if (not (list? tag-path))
                                             (set! tag-path (if (list? cmf) cmf (get-music-folder))))
                                         ))))
                              (let* ((cn (ly:context-name context))
                                     (cid (ly:context-id context))
                                     (path `(,@tag-path ,(o->sym cn)))
                                     (ccid (tree-get context-count path)))
                                (define (topctx context)
                                  (let ((par (ly:context-find context 'Score)))
                                    (if (ly:context? par) (topctx par) context)))
                                (if (not (integer? ccid))(set! ccid 0))
                                (tree-set! context-count path (+ ccid 1))
                                ; (ly:message "~A ~A" ccid path)
                                (set! path `(,@path ,(get-sym ccid)))
                                (set! tag path)
                                (tree-set! edition-tree path
                                  (cons eng
                                    (let* ((c context)
                                           (takt (ly:context-property c 'currentBarNumber))
                                           (mpos (ly:context-property c 'measurePosition)))
                                      (cons takt mpos) )))
                                (set-object-property! eng 'context context)
                                (set-object-property! eng 'tag-path tag-path)
                                (set-object-property! eng 'path path)
                                (set-object-property! eng 'tag-path-idx ccid)

                                (set! context-mods (tree-create (glue-symbol path "/")))
                                (for-each
                                 (lambda (context-edition-sid)
                                   ; (ly:message "~A" context-edition-sid)
                                   (let ((mtree (tree-get-tree mod-tree context-edition-sid)))
                                     (if (tree? mtree)
                                         (tree-walk mtree '()
                                           (lambda (path k val)
                                             (let ((plen (length path)))
                                               (if (and (= plen 3)(list? val)
                                                        (integer? (list-ref path 0))
                                                        (member (list-ref path 2) edition-list))
                                                   (let* ((subpath (list (list-ref path 0)(list-ref path 1)))
                                                          (submods (tree-get context-mods subpath)))
                                                     (tree-set! context-mods subpath
                                                       (if (list? submods) (append submods val) val))
                                                     ))))
                                           ))))
                                 `((,@tag-path ,ctxname)
                                   ,@(if ctxid `(
                                                  (,@tag-path ,ctxid)
                                                  (,@tag-path ,ctxname ,ctxid)
                                                  ) '())
                                   ,path
                                   )
                                 )
                                ; (if (lalily:verbose) (ly:message "looking for editions in ~A" (glue-list path "/")))
                                )))
                           ; paper column interface
                           (paper-column-interface
                            (lambda (engraver grob source-engraver)
                              (if (eq? #t (ly:grob-property grob 'non-musical))
                                  (let ((mods (get-mods)))
                                    (if (list? mods)
                                        (for-each
                                         (lambda (mod)
                                           (cond
                                            ((and (ly:music? mod) (eq? 'LineBreakEvent (ly:music-property mod 'name)))
                                             (let ((break-permission (ly:music-property mod 'break-permission)))
                                               (set! (ly:grob-property grob 'line-break-permission)
                                                     (if (symbol? break-permission) break-permission 'forbid))
                                               ))
                                            ((and (ly:music? mod) (eq? 'PageBreakEvent (ly:music-property mod 'name)))
                                             (let ((break-permission (ly:music-property mod 'break-permission)))
                                               (set! (ly:grob-property grob 'page-break-permission)
                                                     (if (symbol? break-permission) break-permission 'forbid))
                                               ))
                                            ((and (ly:music? mod) (eq? 'PageTurnEvent (ly:music-property mod 'name)))
                                             (let ((break-permission (ly:music-property mod 'break-permission)))
                                               (set! (ly:grob-property grob 'page-turn-permission)
                                                     (if (symbol? break-permission) break-permission 'forbid))
                                               ))
                                            ((and (ly:music? mod) (eq? 'ApplyOutputEvent (ly:music-property mod 'name)))
                                             (let ((proc (ly:music-property mod 'procedure)))
                                               (proc grob context context)
                                               ))
                                            )) mods)))
                                  )))

                           (start-translation-timestep
                            (lambda (trans . recall) ; recall from process-music
                              (let ((takt (ly:context-property context 'currentBarNumber))
                                    (pos (ly:context-property context 'measurePosition))
                                    (modc '()))
                                (define (modc+ mod)(set! modc `(,@modc ,mod)))
                                (set! barnum takt)(set! measurepos pos)
                                (let ((mods (get-mods)))
                                  ;(display path)(display mods)(newline)
                                  (if (list? mods)
                                      (for-each
                                       (lambda (mod)
                                         (cond
                                          ((override? mod)
                                           (if (is-revert mod)
                                               (do-revert context mod)
                                               (do-override context mod))
                                           (modc+ mod))
                                          ((propset? mod)
                                           (do-propset context mod)
                                           (modc+ mod))
                                          ((and (ly:music? mod)(eq? 'KeyChangeEvent (ly:music-property mod 'name)))
                                           (ly:broadcast (ly:context-event-source context)
                                             (ly:make-stream-event
                                              (ly:make-event-class 'key-change-event)
                                              `((pitch-alist . ,(ly:music-property mod 'pitch-alist))
                                                (tonic . ,(ly:music-property mod 'tonic)))))
                                           )
                                          ((apply-context? mod)
                                           (do-apply context mod))
                                          ((ly:context-mod? mod)
                                           (ly:context-mod-apply! context mod)
                                           (modc+ mod))
                                          )) mods)
                                      ))

                                ; warning if start-translation-timestep is not called in first place
                                (if (and (> (length modc) 0)(> (length recall) 0) (eq? #t (car recall)))
                                    (begin
                                     (ly:warning "missing @ ~A ~A ~A" takt pos (glue-list tag "/"))
                                     (for-each (lambda (mod) (ly:warning "---> ~A" mod)) modc)
                                     ))
                                )))

                           (stop-translation-timestep
                            (lambda (trans)
                              (let ((mods (get-mods)))
                                (if (list? mods)
                                    (for-each (lambda (mod)
                                                (cond
                                                 ((and (override? mod)(is-once mod))
                                                  (do-revert context mod))
                                                 ((and (propset? mod)(is-once mod))
                                                  (reset-prop context mod))
                                                 ))
                                      mods))
                                )))

                           (process-music
                            (lambda (trans)
                              (let ((takt (ly:context-property context 'currentBarNumber))
                                    (pos (ly:context-property context 'measurePosition)))
                                ; recall start-translation-timestep, if it is not called already
                                (if (or (not (equal? takt barnum))(not (equal? measurepos pos)))
                                    (start-translation-timestep trans #t))
                                (let ((mods (get-mods)))
                                  (if (list? mods)
                                      (for-each
                                       (lambda (mod)
                                         (cond
                                          ((and (ly:music? mod) (eq? 'TextScriptEvent (ly:music-property mod 'name)))
                                           (let ((grob (ly:engraver-make-grob trans 'TextScript (ly:make-stream-event '(event) `((origin . ,(ly:music-property mod 'origin) ))) ))
                                                 (text (ly:music-property mod 'text))
                                                 (direction (ly:music-property mod 'direction #f)))
                                             (ly:grob-set-property! grob 'text text)
                                             (if direction (ly:grob-set-property! grob 'direction direction))
                                             ))
                                          ((and (ly:music? mod) (eq? 'MarkEvent (ly:music-property mod 'name)))
                                           (let ((grob (ly:engraver-make-grob trans 'RehearsalMark (ly:make-stream-event '(event) `((origin . ,(ly:music-property mod 'origin) ))) ))
                                                 (text (ly:music-property mod 'label)))
                                             (if (not (markup? text))
                                                 (let ((rmi (ly:context-property context 'rehearsalMark))
                                                       (rmf (ly:context-property context 'markFormatter)))
                                                   (if (and (integer? rmi)(procedure? rmf))
                                                       (let ((rmc (ly:context-property-where-defined context 'rehearsalMark)))
                                                         (set! text (rmf rmi rmc))
                                                         (ly:context-set-property! rmc 'rehearsalMark (+ 1 rmi))
                                                         ))))
                                             (ly:grob-set-property! grob 'text text)
                                             ))
                                          ))
                                       mods)))
                                )))

                           (finalize
                            (lambda (trans)
                              (if (eq? 'Score (ly:context-name context))
                                  (let* ((takt (ly:context-property context 'currentBarNumber))
                                         (pos (ly:context-property context 'measurePosition))
                                         )
                                    (ly:message "(~A) finalize ~A (~A ~A)"
                                      (glue-list (editions) ", ")
                                      (glue-list tag "/")
                                      takt (if (ly:moment? pos) (moment->string pos) pos))
                                    (let* ((outname (ly:parser-output-name (*parser*)))
                                           (logfile (format "~A.edition.log" outname)))
                                      (ly:message "writing '~A' ..." logfile)
                                      (with-output-to-file logfile
                                        (lambda()
                                          (display-edition)
                                          (display "<--- mods --->")(newline)
                                          (display-mods)
                                          ))
                                      ))
                                  (set! context-count (tree-create 'context))
                                  )))
                           )
                      `(
                         (initialize . ,initialize)
                         (acknowledgers
                          (paper-column-interface . ,paper-column-interface)
                          )
                         (start-translation-timestep . ,start-translation-timestep)
                         (stop-translation-timestep . ,stop-translation-timestep)
                         (process-music . ,process-music)
                         (finalize . ,finalize)
                         ))))
            eng))
        )
  (set! walk-edition-engravers
        (lambda (proc)
          (tree-walk edition-tree '() ; walk all
            (lambda (path key value)
              (proc path (if (pair? value) (car value) value))
              ) '(empty . #f) '(sort . #f))
          ))

  (set! context-find-edition-engraver
        (lambda (context)
          (let ((peng #f))
            (define (search-peng path eng)
              (if (eqv? (object-property eng 'context) context)
                  (set! peng eng)))
            (if (ly:context? context) (walk-edition-engravers search-peng))
            peng
            )))

  (set! display-edition (lambda () (tree-display edition-tree
                                     '(pathsep . " ")
                                     `(vformat . ,(lambda (p)
                                                    (let* ((m (if (pair? p) (cdr p) p))
                                                           (ctx (if (pair? p) (object-property (car p) 'context) #f))
                                                           (ctxid (if (ly:context? ctx) (ly:context-id ctx) "")))

                                                      (if (and (pair? m)(ly:moment? (cdr m)))
                                                          (format "\"~A\" (~A . ~A)" ctxid (car m)(moment->string (cdr m)))
                                                          (format "\"~A\" ~A" ctxid m))
                                                      )))
                                     )))
  (set! display-mods
        (lambda ()
          (tree-display mod-tree
            '(pathsep . " ")
            `(pformat . ,(lambda (v) (cond
                                      ((ly:moment? v) (moment->string v))
                                      (else (format "~A" v))
                                      )))
            `(vformat . ,(lambda (v)
                           (if (list? v)
                               (glue-list (map (lambda (e)
                                                 (cond
                                                  ((ly:music? e)
                                                   (let ((ann (ly:music-property e 'annotation)))
                                                     (if (annotation? ann)
                                                         (format "[A] ~A: ~A" (markup->string (title ann)) (markup->string (annotation ann)))
                                                         (format "[M] ~A" (ly:music-property e 'name)))
                                                     ))
                                                  (else (format "~A" e)))) v) "\n") (format "~A" v)))))))
  )

;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(define (frac-or-mom? v) (or (fraction? v)(ly:moment? v)))
(define (music-or-contextmod? v) (or (ly:music? v)(ly:context-mod? v)))
(define-public editionMod
  (define-void-function (edition takt pos path mod)
    (string-or-symbol? integer? frac-or-mom? list? music-or-contextmod?)
    "Add modification to edition @ measure moment"
    (if (fraction? pos)(set! pos (ly:make-moment (car pos)(cdr pos))))
    (add-edmod edition takt pos (create-music-path #f path) mod)
    ))

(define (memom? v)
  (and (pair? v)(integer? (car v))
       (let ((cv (cdr v)))
         (if (list? cv)(set! cv (car cv)))
         (or (rational? cv)(frac-or-mom? cv))
         )))
(use-modules (srfi srfi-1))
(define (limemom? v)(and (list? v)(every memom? v)))
(define-public editionModList
  (define-void-function (edition path mod mposl)
    (string-or-symbol? list? music-or-contextmod? limemom?)
    "Add modification to edition at all positions in mposl"
    (let ((path (create-music-path #f path)))
      (for-each
       (lambda (p)
         (let ((takt (car p))
               (pos (cdr p)))
           (if (list? pos)(set! pos (car pos)))
           (if (fraction? pos)(set! pos (fraction->moment pos)))
           (if (rational? pos)
               (set! pos (ly:make-moment (numerator pos)(denominator pos))))
           (add-edmod edition takt pos path mod)
           )) mposl)
      )))

(define (list-or-boolean? v) (or (boolean? v)(list? v)(procedure? v)))
(define-public editionEngraver
  (define-scheme-function (tag)(list-or-boolean?)
    (edition-engraver tag)))

;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

; annotation class
(define-class <annotation> ()
  (piece #:init-value "" #:accessor piece #:setter set-piece! #:init-keyword #:piece)
  (category #:init-value 'TODO #:accessor category #:setter set-category! #:init-keyword #:category)
  (title #:init-value "???" #:accessor title #:setter set-title! #:init-keyword #:title)
  (annotation #:init-value "" #:accessor annotation #:setter set-annotation! #:init-keyword #:annotation)
  (page-ref #:init-value #f #:accessor page-ref #:setter set-page-ref! #:init-keyword #:page-ref)
  (measure #:init-value 0 #:accessor measure #:setter set-measure! #:init-keyword #:measure)
  (position #:init-value (ly:make-moment 0 0) #:accessor position #:setter set-position! #:init-keyword #:position)
  (moment #:init-value (ly:make-moment 0 0) #:accessor moment #:setter set-moment! #:init-keyword #:moment)
  )
(export piece)
(export set-piece!)
(export category)
(export set-category!)
(export title)
(export set-title!)
(export annotation)
(export set-annotation!)
(export page-ref)
(export set-page-ref!)
(export measure)
(export set-measure!)
(export position)
(export set-position!)
(export moment)
(export set-moment!)
; position of annotation as string
(define-method (anno-pos (a <annotation>))
  (let* ((mom (position a))
         (num (ly:moment-main-numerator mom))
         (den (ly:moment-main-denominator mom))
         (gnum (ly:moment-grace-numerator mom))
         (gden (ly:moment-grace-denominator mom)))
    (format "~A, ~A/~A~A" (measure a) num den
      (cond
       ((> gnum 0)(format "+~A:~A" gnum gden))
       ((< gnum 0)(format "~A:~A" gnum gden))
       (else "")
       ))
    ))
(export anno-pos)
; display annotation
(define-method (display (a <annotation>) port)
  (format port "~A: ~A" (anno-pos a) (markup->string (title a))))

; create annotation instance
(define-public (make-anno cat title text . opts)
  (let ((anno (make <annotation> #:category cat #:title title #:annotation text))
        (hasm #f))
    (for-each (lambda (p) (cond ((symbol? p) (set-page-ref! anno p))
                            ((integer? p) (begin (set-measure! anno p)(set! hasm #t)))
                            ((ly:moment? p) (if hasm (set-position! anno p) (set-moment! p))))
                ) opts)
    anno))

; annotation predicate
(define-public (annotation? a) (is-a? a <annotation>))
; annotation comparator
(define-public (annotation<? a1 a2)
  (let* ((clearmup (lambda (m) (if (markup? m) (markup->string m) "")))
         (piece1 (clearmup (piece a1)))
         (piece2 (clearmup (piece a2)))
         (cat1 (category a1))
         (cat2 (category a2))
         (title1 (clearmup (title a1)))
         (title2 (clearmup (title a2)))
         (moment1 (moment a1))
         (moment2 (moment a2)))
    (set! cat1 (if cat1 (format "~A" cat1) ""))
    (set! cat2 (if cat2 (format "~A" cat2) ""))
    (cond
     ((not (string=? piece1 piece2)) (string<? piece1  piece2))
     ((not (string=? cat1 cat2)) (string<? cat1  cat2))
     ((or (ly:moment<? moment1 moment2) (ly:moment<? moment2 moment1)) (ly:moment<? moment1 moment2))
     (else (string<? title1  title2)))
    ))


(define-public (annotations pc) '())
(define-public (add-annotation context annotation) #f)
(define-public (annoCollect context) #f)
(define-public (display-annotations . pcs) #f)

(let ((msgs '())
      (msgc 0)
      (instance 1))
  (define (file-written file) (get-registry-val `(lalily runtime todolog file ,file) #f))
  (define (set-file-written file) (set-registry-val `(lalily runtime todolog file ,file) #t))
  (set! annotations
        (lambda (pc)
          (sort (if (string? pc) (filter (lambda (a) (string=? pc (piece a))) msgs) msgs) annotation<?)))
  (set! display-annotations
        (lambda (. pcs)
          (let ((pct #f))
            (define (display-anns pc)
              (for-each (lambda (a)
                          (if (not (equal? pct (piece a)))
                              (begin
                               (set! pct (piece a))
                               (newline)(display pct)(newline)
                               (display "----------------------")(newline)))
                          (display a)(display ": ")(display (markup->string (annotation a)))(newline))
                (annotations pc))
              )
            (if (> (length pcs) 0)
                (for-each
                 (lambda (pc)
                   (display-anns pc)
                   )
                 pcs)
                (display-anns #f))
            )))
  (set! add-annotation
        (lambda (context annotation pc)
          (if (annotation? annotation)
              (let ((curpos (ly:context-current-moment context))
                    (takt (ly:context-property context 'currentBarNumber))
                    (mpos (ly:context-property context 'measurePosition)))
                (set-piece! annotation pc)
                (set-measure! annotation takt)
                (set-position! annotation mpos)
                (set-moment! annotation curpos)
                (set! msgs (append msgs (list annotation)))
                ))))
  (set! annoCollect
        (lambda (context)
          (let* ((outname (ly:parser-output-name (get-registry-val lalily:registry-parser)))
                 (edeng (context-find-edition-engraver context))
                 (edpath (if edeng (object-property edeng 'path) #f))
                 ; title/instrumentName
                 (pc (if edpath
                         (glue-list edpath " ")
                         (format "~A~3,'0d"
                           (if (> (length (get-music-folder)) 0)
                               (string-append (glue-list (get-music-folder) " ") " internal ") "") instance)
                         ))
                 (printmsgs (lambda()
                              (let ((todofile (format "~A.todo.log" outname pc)))
                                (if (> (length msgs) msgc)
                                    (begin
                                     (set! msgc (length msgs))
                                     (if (not (file-written todofile))
                                         (ly:message "writing '~A'" todofile))
                                     (set-file-written todofile)
                                     (with-output-to-file todofile display-annotations)
                                     ))
                                )))
                 )
            (set! instance (1+ instance))
            (make-engraver
             (listeners ((text-script-event engraver event)
                         (let* ((m (ly:event-property event 'music-cause))
                                (annotation (if (ly:music? m) (ly:music-property m 'annotation #f) #f)))
                           (add-annotation context annotation pc))
                         ))
             ((finalize trans) (printmsgs))
             )))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; music functions

; activate edition
(define-public addEdition
  (define-music-function (edition)(string-or-symbol?)
    "Add edition to edition-list.
Every edition from the global edition-list will be listened for by the edition-engraver."
    (if (string? edition) (set! edition (string->symbol edition)))
    (if (not (memq edition (editions))) (set-editions! `(,@(editions) ,edition)))
    (make-music 'SequentialMusic 'void #t)
    ))

; deactivate edition
(define-public removeEdition
  (define-music-function (edition)(string-or-symbol?)
    "Remove edition from edition-list.
Every edition from the global edition-list will be listened for by the edition-engraver."
    (if (string? edition) (set! edition (string->symbol edition)))
    (set-editions! (delete edition (editions)))
    (make-music 'SequentialMusic 'void #t)
    ))

; set editions
(define-public setEditions
  (define-void-function (editions)(list?)
    "Set edition-list to editions.
Every edition from the global edition-list will be listened for by the edition-engraver.
This will override the previously set list."
    (set-editions! (map (lambda (edition)
                          (cond
                           ((symbol? edition) edition)
                           ((string? edition) (string->symbol edition))
                           (else (string->symbol (format "~A" edition)))
                           )) editions))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; music functions

(define-public text
  (define-music-function (tweaks opts txt)((list? '()) (list? '()) markup?)
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
  (define-music-function (tweaks opts title txt)((list? '()) (list? '()) markup? markup?)
    (let ((defopts `((style . TBD)(annotation . ,(make-anno 'TODO title txt))())))
      (text tweaks (assoc-set-all! defopts opts) title)
      )))

