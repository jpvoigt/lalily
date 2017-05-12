\version "2.19.32"

#(define lalily-relincl-tmp (ly:get-option 'relative-includes))
#(ly:set-option 'relative-includes #t)
\include "../lalily.ly"
#(ly:set-option 'relative-includes lalily-relincl-tmp)

#(define-public (naturalize-pitch p)
   (let ((o (ly:pitch-octave p))
         (a (* 4 (ly:pitch-alteration p)))
         ;; alteration, a, in quarter tone steps,
         ;; for historical reasons
         (n (ly:pitch-notename p)))
     (cond
      ((and (> a 1) (or (eq? n 6) (eq? n 2)))
       (set! a (- a 2))
       (set! n (+ n 1)))
      ((and (< a -1) (or (eq? n 0) (eq? n 3)))
       (set! a (+ a 2))
       (set! n (- n 1))))
     (cond
      ((> a 2) (set! a (- a 4)) (set! n (+ n 1)))
      ((< a -2) (set! a (+ a 4)) (set! n (- n 1))))
     (if (< n 0) (begin (set! o (- o 1)) (set! n (+ n 7))))
     (if (> n 6) (begin (set! o (+ o 1)) (set! n (- n 7))))
     (ly:make-pitch o n (/ a 4))))
#(define-public (naturalize music)
   (let ((es (ly:music-property music 'elements))
         (e (ly:music-property music 'element))
         (p (ly:music-property music 'pitch)))
     (if (pair? es)
         (ly:music-set-property!
          music 'elements
          (map (lambda (x) (naturalize x)) es)))
     (if (ly:music? e)
         (ly:music-set-property!
          music 'element
          (naturalize e)))
     (if (ly:pitch? p)
         (begin
          (set! p (naturalize-pitch p))
          (ly:music-set-property! music 'pitch p)))
     music))

#(define (get-option key options def)
   (let ((default (ly:assoc-get 'default options '() #f)))
     (ly:assoc-get key options (ly:assoc-get key default def #f) #f)
     ))

\registerTemplate lalily.instrument
#(define-music-function (piece options)(list? list?)
   (let ((staff-context (get-option 'staff-context options "Staff"))
         (voice-context (get-option 'voice-context options "Voice"))
         (name (get-option 'name options "instrument"))
         (instrument-name (get-option 'instrument-name options #f))
         (short-name (get-option 'short-name options #f))
         (init-voice (get-option 'init-voice options #f))
         (clef (get-option 'clef options #f))
         (transp (get-option 'transposition options (ly:make-pitch 0 0 0)))
         (natpit (get-option 'naturalize options #f))
         (input-concert-pitch (get-option 'input-concert-pitch options #t))
         (output-concert-pitch (get-option 'output-concert-pitch options #t))
         (staff-mods (get-option 'staff-mods options #f))
         (voice-mods (get-option 'voice-mods options #f))
         (midi-instrument (get-option 'midi-instrument options #f))
         (meta (get-music-deep piece 'meta #f))
         )
     (define (natmus mus) (if natpit (naturalize mus) mus))
     (if (string? clef)
         (set! meta (make-music 'SimultaneousMusic
                      'elements (list #{ \clef #clef #} meta)))
         )
     #{
       \new $staff-context = $name \with {
         $(if (ly:context-mod? staff-mods) staff-mods)
         \consists \editionEngraver $piece
         $(if (string? midi-instrument) #{ \with { midiInstrument = #midi-instrument } #} #{ \with {} #})
         %$(if (string? instrument-name) #{ \with { instrumentName = #instrument-name } #} #{ \with {} #})
         %$(if (string? short-name) #{ \with { instrumentName = #short-name } #} #{ \with {} #})
       } \new $voice-context = $name \with {
         $(if (ly:context-mod? voice-mods) voice-mods)
       } {
         $(if (not output-concert-pitch) #{ \transposition $transp #})
         $(cond
           ((and input-concert-pitch (not output-concert-pitch))
            (natmus #{
              \transpose $transp c' <<
                { #meta }
                { \getMusicDeep {} #(glue-symbol (list name 'global) "-") $(if (ly:music? init-voice) init-voice) \getMusic #'() }
              >>
              #}))
           ((and (not input-concert-pitch) output-concert-pitch)
            (natmus #{
              <<
                { #meta }
                \transpose c' $transp { \getMusicDeep {} #(glue-symbol (list name 'global) "-") $(if (ly:music? init-voice) init-voice) \getMusic #'() }
              >>
              #}))
           ((and (not input-concert-pitch) (not output-concert-pitch))
            #{
              <<
                \transpose c' $transp { #meta }
                { \getMusicDeep {} #(glue-symbol (list name 'global) "-") $(if (ly:music? init-voice) init-voice) \getMusic #'() }
              >>
            #})
           (else
            #{
              <<
                { #meta }
                { \getMusicDeep {} #(glue-symbol (list name 'global) "-") $(if (ly:music? init-voice) init-voice) \getMusic #'() }
              >>
            #})
           )
       }
     #}))

\registerTemplate lalily.instrument.group
#(define-music-function (piece options)(list? list?)
   (let ((groupmod (ly:assoc-get 'groupmod options #f #f))
         (staffs (ly:assoc-get 'staffs options '() #f)))
     #{
       \new StaffGroup \with {
         $(if (ly:context-mod? groupmod) groupmod)
       } $(make-music 'SimultaneousMusic 'elements
            (map (lambda (staff)
                   (let* ((key (assoc-get 'music (cdr staff) (list (car staff))))
                          (opts (assoc-set-all! (get-default-options (create-music-path #f key)) (cdr staff)))
                          (instr (ly:assoc-get 'instrument opts #f #f))
                          (templ (cond
                                  ((symbol? instr) `(.. ,instr))
                                  ((list? instr) `(.. ,@instr))
                                  (else '(..))
                                  )))
                     ;(ly:message "~A -> ~A" piece key)
                     ;(ly:message "~A" (create-music-path #f key))
                     ;(ly:message "~A" (create-template-path #f templ))
                     ;(ly:message "~A" (format-alist opts))
                     #{ \callTemplate #templ #key #opts #}
                     )) staffs))
     #}))

\registerTemplate lalily.instrument.generic
#(define-music-function (piece options)(list? list?)
   (call-template (create-template-path #f '(..)) piece
     (assoc-set-all! options `((name . "melody")
                               (midi-instrument . "oboe")
                               ))))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% woodwind

\registerTemplate lalily.instrument.oboe
#(define-music-function (piece options)(list? list?)
   (call-template (create-template-path #f '(..)) piece
     (assoc-set-all! options `((name . "oboe")
                               (midi-instrument . "oboe")
                               ))))

\registerTemplate lalily.instrument.english-horn
#(define-music-function (piece options)(list? list?)
   (call-template (create-template-path #f '(..)) piece
     (assoc-set-all! options `((name . "english-horn")
                               (transposition . ,(ly:make-pitch -1 3 0))
                               (midi-instrument . "english horn")
                               ))))

\registerTemplate lalily.instrument.sax.sop
#(define-music-function (piece options)(list? list?)
   (call-template (create-template-path #f '(.. ..)) piece
     (assoc-set-all! options `((name . "saxsop")
                               (transposition . ,(ly:make-pitch -1 6 -1/2)) ; b
                               (midi-instrument . "soprano sax")
                               ))))
\registerTemplate lalily.instrument.sax.alt
#(define-music-function (piece options)(list? list?)
   (call-template (create-template-path #f '(.. ..)) piece
     (assoc-set-all! options `((name . "saxalt")
                               (transposition . ,(ly:make-pitch -1 2 -1/2)) ; ees
                               (midi-instrument . "alto sax")
                               ))))

\registerTemplate lalily.instrument.sax.ten
#(define-music-function (piece options)(list? list?)
   (call-template (create-template-path #f '(.. ..)) piece
     (assoc-set-all! options `((name . "saxten")
                               (transposition . ,(ly:make-pitch -2 6 -1/2)) ; b
                               (midi-instrument . "tenor sax")
                               ))))
\registerTemplate lalily.instrument.sax.bar
#(define-music-function (piece options)(list? list?)
   (call-template (create-template-path #f '(.. ..)) piece
     (assoc-set-all! options `((name . "saxbar")
                               (transposition . ,(ly:make-pitch -2 2 -1/2)) ; ees
                               (midi-instrument . "baritone sax")
                               ))))


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% brass

\registerTemplate lalily.instrument.trumpet
#(define-music-function (piece options)(list? list?)
   (call-template (create-template-path #f '(..)) piece
     (assoc-set-all! `((default .
                         ((name . "trumpet")
                          (transposition . ,(ly:make-pitch -1 6 -1/2))
                          (midi-instrument . "trumpet")
                          (output-concert-pitch . #f)
                          )))
       options
       )))
\registerTemplate lalily.instrument.trombone
#(define-music-function (piece options)(list? list?)
   (call-template (create-template-path #f '(..)) piece
     (assoc-set-all! '((default .
                         ((name . "trombone")
                          (midi-instrument . "trombone")
                          (clef . "bass")
                          )))
       options
       )))
\registerTemplate lalily.instrument.tuba
#(define-music-function (piece options)(list? list?)
   (call-template (create-template-path #f '(..)) piece
     (assoc-set-all! '((default .
                         ((name . "tuba")
                          (midi-instrument . "tuba")
                          (clef . "bass")
                          )))
       options
       )))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% string

\registerTemplate lalily.instrument.violin
#(define-music-function (piece options)(list? list?)
   (call-template (create-template-path #f '(..)) piece
     (assoc-set-all! '((name . "violin")
                       (midi-instrument . "violin")
                       ) options)
     ))

\registerTemplate lalily.instrument.viola
#(define-music-function (piece options)(list? list?)
   (call-template (create-template-path #f '(..)) piece
     (assoc-set-all! '((name . "viola")
                       (midi-instrument . "viola")
                       (clef . "alto")
                       ) options)
     ))
\registerTemplate lalily.instrument.cello
#(define-music-function (piece options)(list? list?)
   (call-template (create-template-path #f '(..)) piece
     (assoc-set-all! '((name . "cello")
                       (midi-instrument . "cello")
                       (clef . "bass")
                       ) options)
     ))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% continuo

\registerTemplate lalily.instrument.Continuo
#(define-music-function (piece options)(list? list?)
   (let ((organ #f)
         (do-general (assoc-get 'general options #t #f))
         (sysdelim (assoc-get 'systemStartDelimiter options 'SystemStartBar #f)))
     #{
       \new StaffGroup \with {
         systemStartDelimiter = #sysdelim
         \override SystemStartBracket.collapse-height = #1
         \override SystemStartBar.collapse-height = #1
       } <<
         \callTemplate LY_UP ##t #piece #(assoc-set-all! options
                                           `(,(if organ '(midi-instrument . "drawbar organ") '(dummy . #f))
                                              ;(staff-context . "ContinuoStaff")
                                              ;(voice-context . "ContinuoVoice")
                                              (clef . "bass")
                                              (name . "continuo")
                                              ))
         $(if (and do-general (has-music? (create-music-path #f '(general))))
              #{
                \context Staff = "continuo" \figuremode {
                  \set Staff.figuredBassAlterationDirection = #RIGHT
                  \set Staff.figuredBassPlusDirection = #RIGHT
                  \getMusic general
                }
              #}
              (make-music 'SequentialMusic 'void #t))
       >>
     #}))

