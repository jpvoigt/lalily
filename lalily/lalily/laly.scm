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

(define-module (lalily laly))

(use-modules (ice-9 regex)(srfi srfi-1)(lily)(lalily lascm)(lalily definitions))

(define-public (lalily:verbose)(get-registry-val lalily:registry-verbose))
(define-public (lalily:parser)(get-registry-val lalily:registry-parser))

(define-public (lalily:save-def name val)
  (if (string? name) (set! name (string->symbol name)))
  (if (symbol? name)
      (set-registry-val lalily:registry-parser-defs
        (assoc-set! (get-registry-val lalily:registry-parser-defs '()) name val))
      (ly:warning "~A not a symbol!" name)))

(define-public (extent-size ext diff) (cons (- (car ext) diff) (+ (cdr ext) diff) ))
(define-public (info-message location format . args)
  (if (ly:input-location? location)
      (apply ly:input-message location format args)
      (apply ly:message format args)))

(define-public (location-extract-path location)
  (let* ((loc (car (ly:input-file-line-char-column location)))
         (dirmatch (string-match "(.*/).*" loc))
         (dirname (if (regexp-match? dirmatch) (match:substring dirmatch 1) "./")))
    (normalize-path-string dirname)
    ))

(define-public (la:find-file file)
  (let ((fn (string-append (get-registry-val '(lalily runtime path)) file)))
    (if (not (file-exists? fn)) (set! fn (ly:find-file file)))
    (if (not (and fn (file-exists? fn))) (set! fn (%search-load-path file)))
    (if (and fn (file-exists? fn)) fn #f)
    ))

(define-public (la:find-files dir pattern)
  (let ((ret '())
        (dirname dir))
    (if (= (string-length dirname) 0) (set! dirname (get-registry-val '(lalily runtime path))))
    (if (not (eq? #\/ (string-ref dirname (- (string-length dirname) 1))))
        (set! dirname (string-append dirname "/")))
    (if (or (not (file-exists? dirname)) (not (eq? 'directory (stat:type (stat dirname)))))
        (set! dirname #f))

    (if dirname
        (let* ((dir (opendir dirname))
               (entry (readdir dir)))
          (while (not (eof-object? entry))
            (if (regexp-match? (string-match pattern entry))
                (let ((file (string-append dirname entry)))
                  (set! ret `(,@ret ,file))))
            (set! entry (readdir dir))
            )
          (closedir dir)
          ))
    ret
    ))

(define-public (la:parser-include-file parser file once)
  (let ((reg (get-registry-val '(lalily runtime loaded)))
        (file-path (normalize-path-string (ly:find-file file))))
    (if (not (list? reg)) (set! reg '()))
    (if (or (not once) (not (member file-path reg)))
        (begin
         (if (lalily:verbose) (ly:message "include '~A'" file))
         (ly:parser-include-string parser (format "\\include \"~A\"\n" file))
         (if once (set! reg `(,@reg ,file-path)))))
    (set-registry-val '(lalily runtime loaded) reg)))

(define-public includePattern
  (define-void-function (parser location idir pattern)(string? string?)
    (let ((dirname (string-append (location-extract-path location) idir)))

      (if (or (= (string-length dirname) 0)
              (not (eq? #\/ (string-ref dirname (- (string-length dirname) 1)))))
          (set! dirname (string-append dirname "/")))
      (if (or (not (file-exists? dirname)) (not (eq? 'directory (stat:type (stat dirname)))))
          (set! dirname #f))

      (if dirname (let* ((dir (opendir dirname))
                         (entry (readdir dir)))
                    (while (not (eof-object? entry))
                      (if (regexp-match? (string-match pattern entry))
                          (let ((file (string-append dirname entry)))
                            (ly:parser-include-string parser
                              (format "\\include \"~A\"\n" file))))
                      (set! entry (readdir dir))
                      )
                    (closedir dir)
                    ))
      )))
(define-public includeOncePattern
  (define-void-function (parser location idir pattern)(string? string?)
    (let ((dirname (string-append (location-extract-path location) idir)))

      (if (not (eq? #\. (string-ref dirname 0))) (set! dirname (normalize-path-string dirname)))
      (if (or (= (string-length dirname) 0)
              (not (eq? #\/ (string-ref dirname (- (string-length dirname) 1)))))
          (set! dirname (string-append dirname "/")))
      (if (or (not (file-exists? dirname)) (not (eq? 'directory (stat:type (stat dirname)))))
          (set! dirname #f))

      (if dirname (let* ((dir (opendir dirname))
                         (entry (readdir dir)))
                    (while (not (eof-object? entry))
                      (if (regexp-match? (string-match pattern entry))
                          (let ((file (string-append dirname entry)))
                            (la:parser-include-file parser file #t)))
                      (set! entry (readdir dir))
                      )
                    (closedir dir)
                    ))
      )))


(define-public (lalily-test-location? parser location)
  (let ((outname (ly:parser-output-name parser))
        (locname (car (ly:input-file-line-char-column location))))
    (regexp-match? (string-match (format "^(.*/)?~A\\.i?ly$" outname) locname))
    ))


; register markup for re-instantiation
(define-public (lalily-markup parser location name)
  (let* ((mup-name (string->symbol (format "~A-markup" name)))
         (make-name (string->symbol (format "make-~A" mup-name)))
         (mup (if (defined? mup-name) (primitive-eval mup-name) #f))
         (mkp (if (defined? make-name) (primitive-eval make-name) #f)))
    (if mup (set-registry-val lalily:registry-parser-defs
              `(,@(get-registry-val lalily:registry-parser-defs '()) (,mup-name . ,mup)))
        (info-message location "WARNING: '~A' not found!" mup-name))
    (if mkp (set-registry-val lalily:registry-parser-defs
              `(,@(get-registry-val lalily:registry-parser-defs '()) (,make-name . ,mkp)))
        (if (lalily:verbose)(info-message location "WARNING: '~A' not found!" make-name)))
    ))
(define-public lalilyMarkup
  (define-scheme-function (parser location name)(string?)
    (lalily-markup parser location name)))


(define-public clralist
  (define-void-function (parser location alst)
    (string-or-symbol?)
    (if (string? alst)(set! alst (string->symbol alst)))
    (ly:parser-define! parser alst (list))
    ))
(define-public setalist
  (define-void-function (parser location alst opt val)
    (string-or-symbol? string-or-symbol? scheme?)
    (if (string? alst)(set! alst (string->symbol alst)))
    (if (string? opt)(set! opt (string->symbol opt)))
    (let ((l (ly:parser-lookup parser alst))
          (setv #t))
      (set! l (map (lambda (p)
                     (if (and (pair? p) (equal? (car p) opt))
                         (begin
                          (set! setv #f)
                          (cons opt val))
                         p
                         )) l))
      (if setv (set! l (append l (list (cons opt val)))))
      (ly:parser-define! parser alst l)
      )))
(define-public addalist
  (define-void-function (parser location alst opt val)
    (string-or-symbol? string-or-symbol? scheme?)
    (if (string? alst)(set! alst (string->symbol alst)))
    (if (string? opt)(set! opt (string->symbol opt)))
    (let ((l (ly:parser-lookup parser alst)))
      (set! l (filter (lambda (p) (and (pair? p)(not (equal? (car p) opt)))) l))
      (ly:parser-define! parser alst (append l (list (cons opt val))))
      )))
(define-public remalist
  (define-void-function (parser location alst opt)
    (string-or-symbol? string-or-symbol?)
    (if (string? alst)(set! alst (string->symbol alst)))
    (if (string? opt)(set! opt (string->symbol opt)))
    (let ((l (ly:parser-lookup parser alst)))
      (ly:parser-define! parser alst
        (filter (lambda (p) (and (pair? p)(not (equal? (car p) opt)))) l))
      )))

(define-public (get-a-tree parser location name path)
  (if (string? name) (set! name (string->symbol name)))
  (let ((opts (ly:parser-lookup parser name)))
    (define (getval ol op)
      (let ((sym (car op)))
        (cond
         ((> (length op) 1)
          (let ((al (assoc-get sym ol #f)))
            (if (list? al)
                (getval al (cdr op))
                #f)))
         ((= (length op) 1)
          (assoc-get (car op) ol #f))
         (else #f))))
    (if (list? opts)
        (getval opts path)
        (begin
         (ly:input-warning location "~A is not list (~A)" name opts)
         #f)
        )))
(define (add-a-tree parser location name sympath val assoc-set-append)
  (if (string? name) (set! name (string->symbol name)))
  (let ((opts (ly:parser-lookup parser name)))
    (define (setval ol op)
      (let ((sym (car op))
            (ol (if (list? ol) ol (begin (ly:input-warning location "deleting '~A'" ol) '()))))
        (if (> (length op) 1)
            (let ((al (assoc-get sym ol '())))
              (if (not (list? al))
                  (begin
                   ;(ly:input-warning location "deleting '~A' = '~A'" sym al)
                   (set! al '())
                   ))
              (assoc-set-append ol sym (setval al (cdr op)))
              )
            (let ((ov (assoc-get sym ol #f)))
              ;(if ov (ly:input-warning location "deleting '~A'" ov))
              (assoc-set-append ol sym val)
              )
            )))
    (set! opts (setval opts sympath))
    (ly:parser-define! parser name opts)
    ))
(define (rem-a-tree parser location name sympath)
  (if (string? name) (set! name (string->symbol name)))
  (let ((opts (ly:parser-lookup parser name)))
    (define (remval ol op)
      (let ((sym (car op)))
        (if (> (length op) 1)
            (let ((al (assoc-get sym ol '())))
              (set! al (remval al (cdr op)))
              (if (> (length al) 0)
                  (map (lambda (p) (if (and (pair? p)(equal? (car p) sym)) (cons sym al) p)) ol)
                  (filter (lambda (p) (not (and (pair? p)(equal? (car p) sym)))) ol))
              )
            (filter (lambda (p) (not (and (pair? p)(equal? (car p) sym)))) ol)
            )
        ))
    (set! opts (remval opts sympath))
    (ly:parser-define! parser name opts)
    ))

(define-public clratree clralist)
(define-public getatree
  (define-scheme-function (parser location name sympath)(string-or-symbol? list?)
    (get-a-tree parser location name sympath)))
(define-public addatree
  (define-void-function (parser location name sympath val)(string-or-symbol? list? scheme?)
    (add-a-tree parser location name sympath val
      (lambda (l sym val)
        (append (filter (lambda (p) (not (and (pair? p)(equal? (car p) sym)))) l)
          (list (cons sym val)))))))
(define-public setatree
  (define-void-function (parser location name sympath val)(string-or-symbol? list? scheme?)
    (add-a-tree parser location name sympath val
      (lambda (l sym val) (assoc-set! l sym val)))))
(define-public rematree
  (define-void-function (parser location name sympath)(string-or-symbol? list?)
    (rem-a-tree parser location name sympath)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; styled table of contents
;; toc-sections
;; TODO
(define-public (set-toc-section! text) #f)
(define-public (get-toc-section text) #f)
(let ((toc-section #f))
  (set! set-toc-section! (lambda (text)(set! toc-section text)))
  (set! get-toc-section (lambda ()(begin toc-section)))
  )

