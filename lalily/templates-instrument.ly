\version "2.16.0"

#(define lalily-relincl-tmp (ly:get-option 'relative-includes))
#(ly:set-option 'relative-includes #t)
\include "../lalily.ly"
#(ly:set-option 'relative-includes lalily-relincl-tmp)

\registerTemplate #'(lalily instrument)
#(define-music-function (parser location piece options)(list? list?)
   (let ((name (ly:assoc-get 'name options "instrument" #f))
         (transp (ly:assoc-get 'transposition options (ly:make-pitch 0 0 0) #f))
         (input-concert-pitch (ly:assoc-get 'input-concert-pitch options #t #f))
         (output-concert-pitch (ly:assoc-get 'output-concert-pitch options #t #f))
         (staff-mods (ly:assoc-get 'staff-mods options #f #f))
         (voice-mods (ly:assoc-get 'voice-mods options #f #f)))
     #{
       \new Staff = $name \with {
         $(if (ly:context-mod? staff-mods) staff-mods)
         \consists \editionEngraver $piece
       } \new Voice \with {
         $(if (ly:context-mod? voice-mods) voice-mods)
       } {
         $(if (not output-concert-pitch) #{ \transposition $transp #})
         $(cond
           ((and input-concert-pitch (not output-concert-pitch))
            #{
              \transpose $transp c' <<
                { \getMusicDeep #'meta }
                { \getMusicDeep {} #(glue-symbol (list name 'global) "-") \getMusic #'() }
              >>
            #})
           ((and (not input-concert-pitch) output-concert-pitch)
            #{
              <<
                { \getMusicDeep #'meta }
                \transpose c' $transp { \getMusicDeep {} #(glue-symbol (list name 'global) "-") \getMusic #'() }
              >>
            #})
           ((and (not input-concert-pitch) (not output-concert-pitch))
            #{
              <<
                \transpose c' $transp { \getMusicDeep #'meta }
                { \getMusicDeep {} #(glue-symbol (list name 'global) "-") \getMusic #'() }
              >>
            #})
           (else
            #{
              <<
                { \getMusicDeep #'meta }
                { \getMusicDeep {} #(glue-symbol (list name 'global) "-") \getMusic #'() }
              >>
            #})
           )
       }
     #}))

\registerTemplate #'(lalily instrument group)
#(define-music-function (parser location piece options)(list? list?)
   (let ((groupmod (ly:assoc-get 'groupmod options #f #f))
         (staffs (ly:assoc-get 'staffs options '() #f)))
     #{
       \new StaffGroup \with {
         $(if (ly:context-mod? groupmod) groupmod)
       } $(make-music 'SimultaneousMusic 'elements
            (map (lambda (staff)
                   (let* ((key (assoc-get 'music (cdr staff) (list (car staff))))
                          (opts (assoc-set-all! (get-default-options (create-music-path #f key) location) (cdr staff)))
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% woodwind

\registerTemplate #'(lalily instrument english-horn)
#(define-music-function (parser location piece options)(list? list?)
   (call-template (create-template-path #f '(..)) parser location piece
     (assoc-set-all! options `((name . "english-horn")
                               (transposition . ,(ly:make-pitch -1 3 0))
                               ))))

\registerTemplate #'(lalily instrument sax sop)
#(define-music-function (parser location piece options)(list? list?)
   (call-template (create-template-path #f '(.. ..)) parser location piece
     (assoc-set-all! options `((name . "saxsop")
                               (transposition . ,(ly:make-pitch -1 6 -1/2)) ; b
                               ))))
\registerTemplate #'(lalily instrument sax alt)
#(define-music-function (parser location piece options)(list? list?)
   (call-template (create-template-path #f '(.. ..)) parser location piece
     (assoc-set-all! options `((name . "saxalt")
                               (transposition . ,(ly:make-pitch -1 2 -1/2)) ; ees
                               ))))

\registerTemplate #'(lalily instrument sax ten)
#(define-music-function (parser location piece options)(list? list?)
   (call-template (create-template-path #f '(.. ..)) parser location piece
     (assoc-set-all! options `((name . "saxten")
                               (transposition . ,(ly:make-pitch -2 6 -1/2)) ; b
                               ))))
\registerTemplate #'(lalily instrument sax bar)
#(define-music-function (parser location piece options)(list? list?)
   (call-template (create-template-path #f '(.. ..)) parser location piece
     (assoc-set-all! options `((name . "saxbar")
                               (transposition . ,(ly:make-pitch -2 2 -1/2)) ; ees
                               ))))


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% brass

\registerTemplate #'(lalily instrument trumpet)
#(define-music-function (parser location piece options)(list? list?)
   (call-template (create-template-path #f '(..)) parser location piece
     (assoc-set-all! options `((name . "trumpet")
                               (transposition . ,(ly:make-pitch -1 6 -1/2))
                               ))))
\registerTemplate #'(lalily instrument trombone)
#(define-music-function (parser location piece options)(list? list?)
   (call-template (create-template-path #f '(..)) parser location piece
     (assoc-set-all! options '((name . "trombone")))))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% string

\registerTemplate #'(lalily instrument violin)
#(define-music-function (parser location piece options)(list? list?)
   (call-template (create-template-path #f '(..)) parser location piece
     (assoc-set-all! options '((name . "violin")))))

\registerTemplate #'(lalily instrument viola)
#(define-music-function (parser location piece options)(list? list?)
   (call-template (create-template-path #f '(..)) parser location piece
     (assoc-set-all! options '((name . "viola")))))

