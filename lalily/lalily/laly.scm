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
         (if (lalily:verbose) (ly:message "include: '~A'" file))
         (ly:parser-include-string parser (format "\\include \"~A\"\n" file))
         (if once (set! reg `(,@reg ,file-path)))))
    (set-registry-val '(lalily runtime loaded) reg)))

(define-public includePattern (define-music-function (parser location idir pattern)(string? string?)
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
                                  )
                                (make-music 'SequentialMusic 'void #t)))
(define-public includeOncePattern (define-music-function (parser location idir pattern)(string? string?)
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
                                      )
                                    (make-music 'SequentialMusic 'void #t)))


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
    (let ((l (if (defined? alst) (primitive-eval alst) '()))
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
    (let ((l (if (defined? alst) (primitive-eval alst) '())))
      (set! l (filter (lambda (p) (and (pair? p)(not (equal? (car p) opt)))) l))
      (ly:parser-define! parser alst (append l (list (cons opt val))))
      )))
(define-public remalist
  (define-void-function (parser location alst opt)
    (string-or-symbol? string-or-symbol?)
    (if (string? alst)(set! alst (string->symbol alst)))
    (if (string? opt)(set! opt (string->symbol opt)))
    (let ((l (if (defined? alst) (primitive-eval alst) '())))
      (ly:parser-define! parser alst
        (filter (lambda (p) (and (pair? p)(not (equal? (car p) opt)))) l))
      )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; styled table of contents
;; toc-sections
(define-public (set-toc-section! text) #f)
(define-public (get-toc-section text) #f)
(let ((toc-section #f))
  (set! set-toc-section! (lambda (text)(set! toc-section text)))
  (set! get-toc-section (lambda ()(begin toc-section)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; modify slurs and ties
;; based on David Nalesniks work

(define ((alter-curve offsets) grob)
  ;; get default control-points
  (let ((coords (ly:slur::calc-control-points grob))
        (n 0))
    ;; add offsets to default coordinates
    (define loop (lambda (n)
                   (set-car! (list-ref coords n)
                     (+ (list-ref offsets (* 2 n))
                       (car (list-ref coords n))))
                   (set-cdr! (list-ref coords n)
                     (+ (list-ref offsets (1+ (* 2 n)))
                       (cdr (list-ref coords n))))
                   (if (< n 3)
                       (loop (1+ n)))))
    ;; return altered coordinates
    (loop n)
    coords))

(define ((alter-tie offsets) grob)
  ;; get default control-points
  (let ((coords (ly:tie::calc-control-points grob))
        (n 0))
    ;; add offsets to default coordinates
    (define loop (lambda (n)
                   (set-car! (list-ref coords n)
                     (+ (list-ref offsets (* 2 n))
                       (car (list-ref coords n))))
                   (set-cdr! (list-ref coords n)
                     (+ (list-ref offsets (1+ (* 2 n)))
                       (cdr (list-ref coords n))))
                   (if (< n 3)
                       (loop (1+ n)))))
    ;; return altered coordinates
    (loop n)
    coords))

(define-public shapeSlur
  (define-music-function (parser location offsets) (list?)
    (ly:input-warning location "Deprecated shape-function")
    #{
      \once \override Slur #'control-points = $(alter-curve offsets)
    #}))
(define-public shapePhSlur
  (define-music-function (parser location offsets) (list?)
    (ly:input-warning location "Deprecated shape-function")
    #{
      \once \override PhrasingSlur #'control-points = $(alter-curve offsets)
    #}))
(define-public shapeTie
  (define-music-function (parser location offsets) (list?)
    (ly:input-warning location "Deprecated shape-function")
    #{
      \once \override Tie #'control-points = $(alter-tie offsets)
    #}))

(define-public ySlur
  (define-music-function (parser location dy)(number?)
    (ly:input-warning location "Deprecated shape-function")
    #{
      \once \override Slur #'control-points = $(alter-curve (list 0 0 0 dy 0 dy 0 0))
    #}))
(define-public yPhSlur
  (define-music-function (parser location dy)(number?)
    (ly:input-warning location "Deprecated shape-function")
    #{
      \once \override PhrasingSlur #'control-points = $(alter-curve (list 0 0 0 dy 0 dy 0 0))
    #}))
(define-public yTie
  (define-music-function (parser location dy)(number?)
    (ly:input-warning location "Deprecated shape-function")
    #{
      \once \override Tie #'control-points = $(alter-tie (list 0 0 0 dy 0 dy 0 0))
    #}))
(define-public yySlur
  (define-music-function (parser location dy dz)(number? number?)
    (ly:input-warning location "Deprecated shape-function")
    #{
      \once \override Slur #'control-points = $(alter-curve (list 0 dy 0 (+ dy dz) 0 (+ dy dz) 0 dy))
    #}))
(define-public yyPhSlur
  (define-music-function (parser location dy dz)(number? number?)
    (ly:input-warning location "Deprecated shape-function")
    #{
      \once \override PhrasingSlur #'control-points = $(alter-curve (list 0 dy 0 (+ dy dz) 0 (+ dy dz) 0 dy))
    #}))
(define-public yyTie
  (define-music-function (parser location dy dz)(number? number?)
    (ly:input-warning location "Deprecated shape-function")
    #{
      \once \override Tie #'control-points = $(alter-tie (list 0 dy 0 (+ dy dz) 0 (+ dy dz) 0 dy))
    #}))

(define-public pTie
  (define-music-function (parser location dy)(number?)
    #{
      \once \override Tie #'staff-position = $dy
    #}))


(define-public extendLV
  (define-music-function (parser location further) (number?)
    #{
      \once \override LaissezVibrerTie  #'X-extent = #'(0 . 0)
      \once \override LaissezVibrerTie  #'details #'note-head-gap = $(/ further -2)
      \once \override LaissezVibrerTie  #'extra-offset = $(cons (/ further 2) 0)
    #}))



(define curve-warning-color red)

(define ((offset-control-points offsets function) grob)
  (let ((coords (function grob)))
    (if (null? offsets)
        coords
        (map
         (lambda (x y)
           (coord-translate x y))
         coords offsets))))

(define ((shape-curve offsets location) grob)
  (let* ((orig (ly:grob-original grob))
         (siblings (if (and (ly:grob? orig) (ly:spanner? grob))
                       (ly:spanner-broken-into orig) '() ))
         (total-found (length siblings))
         (function (assoc-get 'control-points
                     (cdr (ly:grob-basic-properties grob))))
         (grob-name
          (assoc-get 'name
            (assoc-get 'meta
              (ly:grob-basic-properties grob)))))

    (define (helper sibs offs)
      (if (and (eq? (car sibs) grob)
               (pair? offs))
          ((offset-control-points (car offs) function) grob)
          (if (pair? offs)
              (helper (cdr sibs) (cdr offs))
              ((offset-control-points '() function) grob))))

    ; standardize input so #'((dx1 . dy1) . . . )
    ; and #'( ((dx1 . dy1) . . . ) ) possible
    (if (not (list? (car offsets)))
        (set! offsets (list offsets)))

    ; warnings
    (if (not (= (length offsets) total-found))
        (if (zero? total-found)
            (if (pair? (cdr offsets))
                (begin
                 (set! (ly:grob-property grob 'color) curve-warning-color)
                 (ly:input-warning location
                   "~a is unbroken, modifications for ~a pieces requested"
                   grob-name (length offsets))))
            (if (eq? (last siblings) grob) ; print warning only once
                (begin
                 (for-each
                  (lambda (piece) (set! (ly:grob-property piece 'color) curve-warning-color))
                  siblings)
                 (ly:input-warning location
                   "~a is broken into ~a pieces, modifications for ~a requested"
                   grob-name total-found (length offsets))))))

    (if (>= total-found 2)
        (helper siblings offsets)
        ((offset-control-points (car offsets) function) grob))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; modify beams

(define-public patBeam
  (define-music-function (parser location n dir)(number? number?)
    #{
      \once \override Beam #'positions = $(cons (- n (* 0.25 dir)) (+ n (* 0.25 dir)))
    #}))
(define-public beamDamp
  (define-music-function (parser location damp)(number?)
    #{
      \once \override Beam #'damping = $damp
    #}))
(define dummy (make-music 'SequentialMusic 'void #t))
(define-public stemBeamLen
  (define-music-function (parser location damp len mus)(number? number? ly:music?)
    #{
      \override Beam #'damping = $damp
      \override Stem #'(details beamed-lengths) = $(list len)
      $mus
      \revert Stem #'details
      \revert Beam #'damping
      \dummy
    #}))

(define (octave-up m octave)
   (let* ((old-pitch (ly:music-property m 'pitch))
          (new-note (ly:music-deep-copy m))
          (new-pitch (ly:make-pitch
                      (+ octave (ly:pitch-octave old-pitch))
                      (ly:pitch-notename old-pitch)
                      (ly:pitch-alteration old-pitch))))
     (set! (ly:music-property new-note 'pitch) new-pitch)
     new-note))

(define (octavize-chord elements t)
   (cond ((null? elements) elements)
     ((eq? (ly:music-property (car elements) 'name) 'NoteEvent)
      (cons (car elements)
        (cons (octave-up (car elements) t)
          (octavize-chord (cdr elements) t))))
     (else (cons (car elements) (octavize-chord (cdr elements ) t)))))

(define (octavize music t)
   (cond
    ((eq? (ly:music-property music 'name) 'EventChord)
     (ly:music-set-property! music 'elements
       (octavize-chord
        (map (lambda (e) (if (eq? (ly:music-property music 'name) 'EventChord)
                             (let ((elms (ly:music-property e 'elements)))
                               (if (and (list? elms)(> (length elms) 0)) (car elms) e))
                             e))
          (ly:music-property music 'elements)) t)))
    ((eq? (ly:music-property music 'name) 'NoteEvent)
     (let ((artics (ly:music-property music 'articulations)))
       (ly:music-set-property! music 'articulations '())
       (set! music (make-music 'EventChord 'elements `(,music ,(octave-up music t) ,@artics) ))
       ))
    )
   music)

(define-public makeOctaves
   (define-music-function (parser location arg mus) (integer? ly:music?)
     (music-map (lambda (x) (octavize x arg)) mus)))
