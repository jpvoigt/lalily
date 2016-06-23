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

(use-modules (lalily laly)(lalily lascm)(lalily markup)(lalily store))

(re-export lalily:verbose)

(re-export location-extract-path)
(define-public (file-path location rel)
  (let ((dir (location-extract-path location)))
    (normalize-path-string (string-append dir rel))))
(define-public filePath
  (define-scheme-function (rel)(string?)
    (file-path (*location*) rel)))


(re-export extent-size)
(re-export info-message)
(define (return-music val)(if (ly:music? val) val (make-music 'SequentialMusic 'void #t)))
(define-public displayMessage
  (define-music-function (format val)(string? scheme?)
    (ly:message format val)
    (return-music val)))
(define-public inputMessage
  (define-music-function (format val)(string? scheme?)
    (ly:input-message (*location*) format val)
    (return-music val)))
(define-public displayObject
  (define-music-function (val)(scheme?)
    (display val)
    (return-music val)))
(define-public displayLine
  (define-music-function (val)(scheme?)
    (display val)(newline)
    (return-music val)))
(define-public writeObject
  (define-music-function (val)(scheme?)
    (write val)
    (return-music val)))
(define-public writeLine
  (define-music-function (val)(scheme?)
    (write-line val)
    (return-music val)))


(define (not-null? v)(not (null? v)))

(re-export lalily:save-def)
(define-public parserDefine
  (define-void-function (name val)(string-or-symbol? not-null?)
    (if (string? name) (set! name (string->symbol name)))
    (ly:parser-define! name val)
    (lalily:save-def name val)))

(define-public parserDefineMusic
  (define-void-function (name mus)(string-or-symbol? ly:music?)
    (parserDefine name mus)))
(define-public parserDefineMarkup
  (define-void-function (name mus)(string-or-symbol? markup?)
    (parserDefine name mus)))

(re-export lalily-markup)
(re-export lalilyMarkup)


(re-export clralist)
(re-export setalist)
(re-export addalist)
(re-export remalist)
(re-export clratree)
(re-export setatree)
(re-export addatree)
(re-export rematree)

; do something anywhere
(define-public exec (define-void-function (mus)(scheme?)))

; execute music-lambda(parser location)
(define-public execMusic
  (define-music-function (proc)(procedure?)
    (let ((mus (proc (*parser*) (*location*))))
      (cond ((ly:music? mus) mus)
        (else (make-music 'SequentialMusic 'void #t)))
      )))

(re-export la:parser-include-file)

(define-public includeOnceIfExists
  (define-void-function (file)(string?)
    (if (file-exists? file)
        (la:parser-include-file file #t))
    ))

(define-public includeIfAbsent
  (define-void-function (sym file)(symbol? string?)
    (if (not (defined? sym))
        (ly:parser-include-string (format "\\include \"~A\"\n" file)))
    ))

(define-public includeRelative
  (define-void-function (file)(string?)
    (let ((filename (string-append (location-extract-path (*location*)) file)))
      (ly:parser-include-string (format "\\include \"~A\"\n" filename))
      )))
(define-public includeRelIf
  (define-void-function (file proc)(string? procedure?)
    (if (proc (*parser*) (*location*))
        (includeRelative file))
    ))

(re-export includePattern)
(re-export includeOncePattern)
(define-public includeLocal
  (define-void-function (file)(string?)
    (let ((outname (format "~A.ly" (ly:parser-output-name (*parser*))))
          (locname (car (ly:input-file-line-char-column location))))
      (if (or (string=? outname locname) (string-suffix? outname locname))
          (ly:parser-include-string (format "\\include \"~A\"\n" file)))
      )))
(define-public executeLocal
  (define-void-function (fn)(procedure?)
    (let ((outname (format "~A.ly" (ly:parser-output-name (*parser*))))
          (locname (car (ly:input-file-line-char-column (*location*)))))
      (if (or (string=? outname locname)(string-suffix? outname locname))
          (fn))
      )))

(re-export lalily-test-location?)
(define-public bookpartAdd
  (define-void-function (bookpart)(ly:book?)
    (let ((book (ly:parser-lookup '$current-book)))
      ;(set-book-headers! bookpart (assoc-get 'header (get-music-folder-options location) '()))
      (if book
          (ly:book-add-bookpart! book bookpart)
          (collect-bookpart-for-book bookpart)
          ))))
(define-public bookpartIf
  (define-void-function (proc bookpart)
    ((procedure? lalily-test-location?) ly:book?)
    (if (proc (*parser*) (*location*))
        (bookpartAdd bookpart)
        )))

(define-public (set-book-headers! book header)
  (let ((bookhead (ly:book-header book)))
    (if (or (not bookhead)(list? bookhead))(begin (set! bookhead (make-module)) (ly:book-set-header! book bookhead)))
    (if (not (list? header))
        (set! header (assoc-get 'header (get-music-folder-options) '())))
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
  (define-void-function (size)(number?)
    (set-registry-val lalily:paper:global-staff-size size)
    (set-global-staff-size size)))
(define-public setLocalStaffSize
  (define-void-function (size)(number?)
    (if ((get-registry-val lalily:test-predicate lalily-test-location?) (*parser*) (*location*))
        (ly:music-function-exec setGlobalStaffSize size)
        )))

(define-public midiTempo
  (define-music-function (frac)(fraction?)
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
  (define-music-function (text) (markup?)
    (set-toc-section! text)(make-music 'SequentialMusic 'void #t)))
(define-public tocItem
  (define-music-function (text) (markup?)
    (add-toc-item! 'tocItemMarkup (markup text #:hspace 1))))
(define-public tocCollection
  (define-music-function (text) (markup?)
    (add-toc-item! 'tocCollMarkup (markup text #:hspace 1))))
(define-public tocPart
  (define-music-function (text) (markup?)
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
  (define-music-function (frac beat-structure count)(fraction? (list? '()) integer?)
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
         (ly:make-duration (ly:intlog2 den) 0 (* nom count) 1)))))))

(define-public taktSkip
  (define-music-function (frac count)(fraction? integer?)
    (let ((nom (car frac))
          (den (cdr frac)))
      (make-music
       'SkipEvent
       'duration
       (ly:make-duration (ly:intlog2 den) 0 (* nom count) 1)))))

(define-public taktRest
  (define-music-function (frac count)(fraction? integer?)
    (let ((nom (car frac))
          (den (cdr frac)))
      (make-music
       'MultiMeasureRestMusic
       'duration
       (ly:make-duration (ly:intlog2 den) 0 (* nom count) 1)))))

(define-public inPartial
  (define-music-function
   (pos mus)
   (fraction? ly:music?)
   #{
     \set Score.measurePosition = $(ly:make-moment (- (car pos)) (cdr pos))
     $mus
   #}
   ))

(define-public filterMusic
  (define-music-function (events music) (list? ly:music?)
    (let ((filterEvent (lambda (event)
                         (let ( (eventname (ly:music-property  event 'name))
                                (ret #t) )
                           (for-each (lambda (n) (set! ret (and ret (not (eq? eventname n))))) events)
                           ret
                           )) ))
      (music-filter filterEvent music)
      )))

(define-public anno
  (define-music-function (style text)(symbol? string?)
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
  (define-music-function (dir sym text)((number? 1) (symbol? 'artic) markup?)
    (make-music 'TextScriptEvent 'direction dir 'text
      (markup #:line (#:style sym text)))))
(define-public dedic
  (define-music-function (dir text)((number? 1) markup?)
    (make-music 'TextScriptEvent 'direction dir 'dedic (string->symbol (markup->string text)) 'text
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
  (define-scheme-function (fac proc)(number-pair? procedure?)
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
(define-public markDaX
  (define-music-function (eo text)((number-pair? '(0 . 0)) markup?)
    #{
      \once \override Score.RehearsalMark #'break-visibility = ##(#t #t #f)
      \once \override Score.RehearsalMark #'self-alignment-X = #RIGHT
      \once \override Score.RehearsalMark #'direction = #DOWN
      \once \override Score.RehearsalMark #'extra-offset = #eo
      \mark \markup { \small \italic $text }
    #}))
(define-public markFine
  (define-music-function (eo)(number-pair?)
    #{
      \once \override Score.RehearsalMark #'break-visibility = ##(#t #t #f)
      \once \override Score.RehearsalMark #'self-alignment-X = #RIGHT
      \once \override Score.RehearsalMark #'direction = #DOWN
      \once \override Score.RehearsalMark #'extra-offset = #eo
      \mark \markup { \small \italic "fine." }
    #}))
(define-public markDCFine
  (define-music-function (eo)(number-pair?)
    #{
      \once \override Score.RehearsalMark #'break-visibility = ##(#t #t #f)
      \once \override Score.RehearsalMark #'self-alignment-X = #RIGHT
      \once \override Score.RehearsalMark #'direction = #DOWN
      \once \override Score.RehearsalMark #'extra-offset = $eo
      \mark \markup { \small \italic "d.c. al fine" }
    #}))
(define-public markDaCapo
  (define-music-function (eo)(number-pair?)
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

