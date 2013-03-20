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

(use-modules (lalily laly)(lalily lascm)(lalily markup))

(re-export lalily:verbose)
(re-export lalily:parser)

(define-public (ly:music-function-exec fun parser location . args)
  (apply (ly:music-function-extract fun) parser location args))

(re-export extent-size)
(re-export info-message)
(define-public displayMessage
  (define-music-function (parser location format val)(string? scheme?)
    (ly:message format val)
    (if (ly:music? val)
        val
        (make-music 'SequentialMusic 'void #t)
        )
    ))
(define-public inputMessage
  (define-music-function (parser location format val)(string? scheme?)
    (ly:input-message location format val)
    (if (ly:music? val)
        val
        (make-music 'SequentialMusic 'void #t)
        )
    ))

(define (not-null? v)(not (null? v)))
(define (symbol-or-string? v)(or (symbol? v)(string? v)))

(re-export lalily:save-def)
(define-public parserDefine
  (define-music-function (parser location name val)(symbol-or-string? not-null?)
    (if (string? name) (set! name (string->symbol name)))
    (ly:parser-define! parser name val)
    (lalily:save-def name val)
    (make-music 'SequentialMusic 'void #t)))

(define-public parserDefineMusic
  (define-music-function (parser location name mus)(symbol-or-string? ly:music?)
    ((ly:music-function-extract parserDefine) parser location name mus)
    (make-music 'SequentialMusic 'void #t)))
(define-public parserDefineMarkup
  (define-music-function (parser location name mus)(symbol-or-string? markup?)
    ((ly:music-function-extract parserDefine) parser location name mus)
    (make-music 'SequentialMusic 'void #t)))
(define-public parserDefineScheme
  (define-music-function (parser location name mus)(symbol-or-string? scheme?)
    ((ly:music-function-extract parserDefine) parser location name mus)
    (make-music 'SequentialMusic 'void #t)))

(re-export lalily-markup)
(re-export lalilyMarkup)


(re-export clralist)
(re-export setalist)
(re-export addalist)
(re-export remalist)

; do something anywhere
(define-public exec (define-music-function (parser location mus)(scheme?)
                      (make-music 'SequentialMusic 'void #t)))

; execute music-lambda(parser location)
(define-public execMusic
  (define-music-function (parser location proc)(procedure?)
    (let ((mus (proc parser location)))
      (cond ((ly:music? mus) mus)
        (else (make-music 'SequentialMusic 'void #t)))
      )))

(re-export la:parser-include-file)

(define-public includeOnceIfExists
  (define-music-function (parser location file)(string?)
    (if (file-exists? file)
        (la:parser-include-file parser file #t))
    (make-music 'SequentialMusic 'void #t)))

(define-public includeIfAbsent
  (define-music-function (parser location sym file)(symbol? string?)
    (if (not (defined? sym))
        (ly:parser-include-string parser (format "\\include \"~A\"\n" file)))
    (make-music 'SequentialMusic 'void #t)))

(define-public includeRelIf
  (define-music-function (parser location file proc)(string? procedure?)
    (let ((filename (string-append (location-extract-path location) file)))
      (if (proc parser location) (ly:parser-include-string parser (format "\\include \"~A\"\n" filename)))
      (make-music 'SequentialMusic 'void #t))))

(re-export includePattern)
(re-export includeOncePattern)
(define-public includeLocal
  (define-music-function (parser location file)(string?)
    (let ((outname (format "~A.ly" (ly:parser-output-name parser)))
          (locname (car (ly:input-file-line-char-column location))))
      (if (or (string=? outname locname) (string-suffix? outname locname))
          (ly:parser-include-string parser (format "\\include \"~A\"\n" file)))
      (make-music 'SequentialMusic 'void #t))))
(define-public executeLocal
  (define-music-function (parser location fn)(procedure?)
    (let ((outname (format "~A.ly" (ly:parser-output-name parser)))
          (locname (car (ly:input-file-line-char-column location))))
      (if (or (string=? outname locname)(string-suffix? outname locname))
          (fn))
      (make-music 'SequentialMusic 'void #t))))

(re-export lalily-test-location?)
(define-public bookpartIf
  (define-void-function (parser location proc bookpart)((procedure? lalily-test-location?) ly:book?)
    (if (proc parser location)
        (begin
         ;(set-book-headers! bookpart (assoc-get 'header (get-music-folder-options location) '()))
         (collect-bookpart-for-book parser bookpart)
         ))
    ))

(define-public (set-book-headers! book header)
  (let ((bookhead (ly:book-header book)))
    (if (or (not bookhead)(list? bookhead))(begin (set! bookhead (make-module)) (ly:book-set-header! book bookhead)))
    (if (not (list? header)) (set! header (assoc-get 'header (get-music-folder-options
                                                              (if (ly:input-location? header) header #f)) '())))
    (for-each (lambda (p)
                (if (pair? p)
                    (let ((key (car p))
                          (val (cdr p)))
                      (module-define! bookhead key val)))) header)
    ))
(define-public (set-score-headers! score header)
  (let ((scorehead (ly:score-header score)))
    (if (or (not scorehead)(list? scorehead))(let ((mod (make-module))) (set! scorehead mod) (ly:score-set-header! score scorehead)))
    (if (not (list? header)) (set! header (assoc-get 'header (get-music-folder-options
                                                              (if (ly:input-location? header) header #f)) '())))
    (for-each (lambda (p)
                (if (pair? p)
                    (let ((key (car p))
                          (val (cdr p)))
                      (module-define! scorehead key val)))) header)
    ))

(define-public setGlobalStaffSize
  (define-void-function (parser location size)(number?)
    (set-global-staff-size size)))

(define-public midiTempo
  (define-music-function (parser location frac)(fraction?)
    (make-music 'ContextSpeccedMusic
      'context-type 'Score
      'element (make-music
                'PropertySet
                'value (ly:make-moment (car frac) (cdr frac) 0 1)
                'symbol 'tempoWholesPerMinute))
    ))

;;;;;;;;;;;;;;;;;;
;; toc sections

(re-export set-toc-section!)
(re-export get-toc-section)
(define-public setTocSection
  (define-music-function (parser location text) (markup?)
    (set-toc-section! text)(make-music 'SequentialMusic 'void #t)))
(define-public tocItem
  (define-music-function (parser location text) (markup?)
    (add-toc-item! 'tocItemMarkup (markup text #:hspace 1))))
(define-public tocCollection
  (define-music-function (parser location text) (markup?)
    (add-toc-item! 'tocCollMarkup (markup text #:hspace 1))))
(define-public tocPart
  (define-music-function (parser location text) (markup?)
    (begin
     (if (get-toc-section) (add-toc-item! 'tocCollMarkup (markup (get-toc-section) #:hspace 1)))
     (set-toc-section! #f)
     (add-toc-item! 'tocPartMarkup (markup text #:hspace 1)))))

;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;%%% merge rests
(define (rest-score r)
  (let ((score 0)
        (yoff (ly:grob-property-data r 'Y-offset))
        (sp (ly:grob-property-data r 'staff-position)))
    (if (number? yoff)
        (set! score (+ score 2))
        (if (eq? yoff 'calculation-in-progress)
            (set! score (- score 3))))
    (and (number? sp)
         (<= 0 2 sp)
         (set! score (+ score 2))
         (set! score (- score (abs (- 1 sp)))))
    score))

(define (merge-rests-on-positioning grob)
  (let* ((can-merge #f)
         (elts (ly:grob-object grob 'elements))
         (num-elts (and (ly:grob-array? elts)
                        (ly:grob-array-length elts)))
         (two-voice? (= num-elts 2)))
    (if two-voice?
        (let* ((v1-grob (ly:grob-array-ref elts 0))
               (v2-grob (ly:grob-array-ref elts 1))
               (v1-rest (ly:grob-object v1-grob 'rest))
               (v2-rest (ly:grob-object v2-grob 'rest)))
          (and
           (ly:grob? v1-rest)
           (ly:grob? v2-rest)
           (let* ((v1-duration-log (ly:grob-property v1-rest 'duration-log))
                  (v2-duration-log (ly:grob-property v2-rest 'duration-log))
                  (v1-dot (ly:grob-object v1-rest 'dot))
                  (v2-dot (ly:grob-object v2-rest 'dot))
                  (v1-dot-count (and (ly:grob? v1-dot)
                                     (ly:grob-property v1-dot 'dot-count -1)))
                  (v2-dot-count (and (ly:grob? v2-dot)
                                     (ly:grob-property v2-dot 'dot-count -1))))
             (set! can-merge
                   (and
                    (number? v1-duration-log)
                    (number? v2-duration-log)
                    (= v1-duration-log v2-duration-log)
                    (eq? v1-dot-count v2-dot-count)))
             (if can-merge
                 ;; keep the rest that looks best:
                 (let* ((keep-v1? (>= (rest-score v1-rest)
                                      (rest-score v2-rest)))
                        (rest-to-keep (if keep-v1? v1-rest v2-rest))
                        (dot-to-kill (if keep-v1? v2-dot v1-dot)))
                   ;; uncomment if you're curious of which rest was chosen:
                   ;;(ly:grob-set-property! v1-rest 'color green)
                   ;;(ly:grob-set-property! v2-rest 'color blue)
                   (ly:grob-suicide! (if keep-v1? v2-rest v1-rest))
                   (if (ly:grob? dot-to-kill)
                       (ly:grob-suicide! dot-to-kill))
                   (ly:grob-set-property! rest-to-keep 'direction 0)
                   (ly:rest::y-offset-callback rest-to-keep)))))))
    (if can-merge
        #t
        (ly:rest-collision::calc-positioning-done grob))))

(define merge-multi-measure-rests-on-Y-offset
  ;; Call this to get the 'Y-offset of a MultiMeasureRest.
  ;; It keeps track of other MultiMeasureRests in the same NonMusicalPaperColumn
  ;; and StaffSymbol. If two are found, delete one and return 1 for Y-offset of
  ;; the other one.
  (let ((table (make-weak-key-hash-table)))
    (lambda (grob)
      (let* ((ssymb (ly:grob-object grob 'staff-symbol))
             (nmcol (ly:grob-parent grob X))
             (ssymb-hash (or (hash-ref table ssymb)
                             (hash-set! table ssymb (make-hash-table))))
             (othergrob (hash-ref ssymb-hash nmcol)))
        (if (ly:grob? othergrob)
            (begin
             ;; Found the other grob in this staff/column,
             ;; delete it and move ours.
             (ly:grob-suicide! othergrob)
             (hash-remove! ssymb-hash nmcol)
             1) ; was 0
            (begin
             ;; Just save this grob and return the default value.
             (hash-set! ssymb-hash nmcol grob)
             (ly:staff-symbol-referencer::callback grob)))))))

(define-public mergeRestsOn #{
  \override Staff.RestCollision #'positioning-done = #merge-rests-on-positioning
  \override Staff.MultiMeasureRest #'Y-offset = #merge-multi-measure-rests-on-Y-offset
  #})
(define-public mergeRestsOff #{
  \revert Staff.RestCollision #'positioning-done
  \revert Staff.MultiMeasureRest #'Y-offset
  #})
  (define-public mergeRests #{ \layout {
    \context {
      \Staff
      \override RestCollision #'positioning-done = #merge-rests-on-positioning
      \override MultiMeasureRest #'Y-offset = #merge-multi-measure-rests-on-Y-offset
    }
} #})

;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(define-public taktMeta
  (define-music-function (parser location frac beat-structure count)(fraction? (list? '()) integer?)
    (let ((nom (car frac))
          (den (cdr frac)))
      (make-music
       'SequentialMusic
       'elements
       (list
        (make-music 'TimeSignatureMusic
          'beat-structure beat-structure
          'denominator den
          'numerator nom)
        (make-music
         'SkipEvent
         'duration
         (ly:make-duration (inexact->exact (/ (log den)(log 2))) 0 (* nom count) 1)))))))

(define-public taktSkip
  (define-music-function (parser location frac count)(fraction? integer?)
    (let ((nom (car frac))
          (den (cdr frac)))
      (make-music
       'SkipEvent
       'duration
       (ly:make-duration (inexact->exact (/ (log den)(log 2))) 0 (* nom count) 1)))))

(define-public inPartial
  (define-music-function
   (parser location pos mus)
   (fraction? ly:music?)
   #{
     \set Score.measurePosition = $(ly:make-moment (- (car pos)) (cdr pos))
     $mus
   #}
   ))

(define-public filterMusic
  (define-music-function (parser location events music) (list? ly:music?)
    (let ((filterEvent (lambda (event)
                         (let ( (eventname (ly:music-property  event 'name))
                                (ret #t) )
                           (for-each (lambda (n) (set! ret (and ret (not (eq? eventname n))))) events)
                           ret
                           )) ))
      (music-filter filterEvent music)
      )))

(define-public anno
  (define-music-function (parser location style text)(symbol? string?)
    (make-music 'TextScriptEvent 'text (markup #:style style text))
    ))

(define-public (extract-pitch-diff mus)
  (let ((pitches (list)))
    (music-map (lambda (m)
                 (if (and m (ly:music? m)(eq? (ly:music-property m 'name) 'NoteEvent))
                     (let ((pitch (ly:music-property m 'pitch)))
                       (if (ly:pitch? pitch)
                           (set! pitches (append pitches (list pitch))))))) mus)
    (if (>= (length pitches) 2)
        (ly:pitch-diff (cadr pitches) (car pitches))
        (ly:make-pitch 0 0 0) )
    ))


(setstyle 'dynamic (markup #:italic #:fromproperty 'style:text))
(setstyle 'artic (markup #:italic #:fromproperty 'style:text))
(setstyle 'dedic (markup #:italic #:bold #:fromproperty 'style:text))
(setstyle 'warning (markup #:bold #:larger #:with-color red #:fromproperty 'style:text))

(define-public artic
  (define-music-function (parser location dir sym text)((number? 1) (symbol? 'artic) markup?)
    (make-music 'TextScriptEvent 'direction dir 'text
      (markup #:line (#:style sym text)))))
(define-public dedic
  (define-music-function (parser location dir text)((number? 1) markup?)
    (make-music 'TextScriptEvent 'direction dir 'text
      (markup #:line (#:style 'dedic text)))))

;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
; stencil utils

(define-public (create-scale-stencil fac proc)
  (let ((sx (car fac))
        (sy (cdr fac)))
    (if (number-pair? fac)
        (lambda (grob)
          (ly:stencil-scale (proc grob) sx sy))
        proc)))
(define-public createScaleStencil
  (define-scheme-function (parser location fac proc)(number-pair? procedure?)
    (create-scale-stencil fac proc)))

(define-public (make-stencil-rboxer thickness padding callback)
  "Return function that adds a box around the grob passed as argument."
  (lambda (grob)(let ((m (callback grob)))
                  (ly:stencil-add (rounded-box-stencil m thickness padding (* 10 padding)) m))))

;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
; utils

(define-public markFerm #{
  \once \override Score.RehearsalMark #'break-visibility = ##(#t #t #f)
  \mark \markup { \musicglyph #"scripts.ufermata" }
  #})
(define-public markFine #{
  \once \override Score.RehearsalMark #'break-visibility = ##(#t #t #f)
  \once \override Score.RehearsalMark #'self-alignment-X = #RIGHT
  \once \override Score.RehearsalMark #'direction = #DOWN
  \once \override Score.RehearsalMark #'extra-offset = #'(0 . -2)
  \mark \markup { \small \italic "fine." }
  #})
(define-public markDCFine
  (define-music-function (parser location eo)(number-pair?)
    #{
      \once \override Score.RehearsalMark #'break-visibility = ##(#t #t #f)
      \once \override Score.RehearsalMark #'self-alignment-X = #RIGHT
      \once \override Score.RehearsalMark #'direction = #DOWN
      \once \override Score.RehearsalMark #'extra-offset = $eo
      \mark \markup { \small \italic "d.c. al fine" }
    #}))
(define-public markDaCapo
  (define-music-function (parser location eo)(number-pair?)
    #{
      \once \override Score.RehearsalMark #'break-visibility = ##(#t #t #f)
      \once \override Score.RehearsalMark #'self-alignment-X = #RIGHT
      \once \override Score.RehearsalMark #'direction = #DOWN
      \once \override Score.RehearsalMark #'extra-offset = $eo
      \mark \markup { \small \italic "da capo" }
    #}))

(define-public fullMelisma #{ \set melismaBusyProperties = #'(melismaBusy slurMelismaBusy tieMelismaBusy beamMelismaBusy) #})
(define-public slurMelisma #{ \set melismaBusyProperties = #'(melismaBusy slurMelismaBusy tieMelismaBusy) #})
(define-public stopMelisma #{ \set melismaBusyProperties = #'() #})


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; re-exports
(re-export shapeSlur)
(re-export shapePhSlur)
(re-export shapeTie)
(re-export ySlur)
(re-export yPhSlur)
(re-export yTie)
(re-export yySlur)
(re-export yyPhSlur)
(re-export yyTie)
(re-export pTie)
(re-export extendLV)
(re-export patBeam)
(re-export beamDamp)
(re-export stemBeamLen)
(re-export makeOctaves)

(define-public shy
  (let ((shy-type? (lambda (v) (or (number? v)
                                   (number-pair? v)
                                   (and
                                    (list? v)
                                    (every (lambda (x) (or (number? x)(number-pair? x))) v)
                                    )
                                   ))
          ))
    (define-music-function (parser location grob dy)(string? shy-type?)
      (let ((shape-fun (ly:music-function-extract shape))
            (mod-fun (lambda (m) (cond ((number-pair? m)
                                        (let ((dy (car m))(dz (cdr m)))
                                          `((0 . ,dy)(0 . ,(+ dy dz))(0 . ,(+ dy dz))(0 . ,dy))
                                          ))
                                   ((number? m)
                                    `((0 . 0)(0 . ,m)(0 . ,m)(0 . 0))
                                    )
                                   (else (ly:input-warning location "type??? ~A ~A ~A" m grob dy) '((0 . 0)(0 . 0)(0 . 0)(0 . 0)))
                                   )
                       )))
        (shape-fun parser location grob
          (if (list? dy)
              (map (lambda (y) (mod-fun y)) dy)
              (mod-fun dy)
              )
          )))
    ))
(define-public stretch
  (let ((nol? (lambda (v)(or (number? v)(and (list v)(every number? v))))))
    (define-music-function (parser location grob xf)(string? nol?)
      #{ \shape $grob #(if (list? xf)
                           (map (lambda (x)
                                  (if (> xf 0)
                                      `((0 . 0)(,x . 0)(,(* 2 x) . 0)(,(* 3 x) . 0))
                                      `((,(* 3 x) . 0)(,(* 2 x) . 0)(,(* 1 x) . 0)(0 . 0))
                                      )
                                  ) xf)
                           (if (> xf 0)
                               `((0 . 0)(,xf . 0)(,(* 2 xf) . 0)(,(* 3 xf) . 0))
                               `((,(* 3 xf) . 0)(,(* 2 xf) . 0)(,(* 1 xf) . 0)(0 . 0))
                               )
                           )
      #})))

(define-public stretchPunch
  (let ((nol? (lambda (v)(or (number? v)(and (list? v)(every number? v)))))
        (nop? (lambda (v)(or (number? v)(and (list? v)(every (lambda (y) (or (number? y)(number-pair? y))) v))))))
    (define-music-function (parser location grob xf yf)(string? nol? nop?)
      (if (not (list? xf)) (set! xf (list xf)))
      (if (not (list? yf)) (set! yf (list yf)))
      #{ \shape $grob $(map (lambda (x y)
                              (let* ((oy (if (pair? y) (car y) 0))
                                     (iy (+ oy (if (pair? y) (cdr y) y))))
                                (if (> x 0)
                                    `((0 . ,oy)(,x . ,iy)(,(* 2 x) . ,iy)(,(* 3 x) . ,oy))
                                    `((,(* 3 x) . ,oy)(,(* 2 x) . ,iy)(,(* 1 x) . ,iy)(0 . ,oy))
                                    )
                                )) xf yf)
      #})))
