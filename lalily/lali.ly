%%%% This file is part of lalily, an extension to lilypond <http://www.lilypond.org/>.
%%%%
%%%% Copyright (C) 2011--2012 Jan-Peter Voigt <jp.voigt@gmx.de>
%%%%
%%%% lalily is free software: you can redistribute it and/or modify
%%%% it under the terms of the GNU General Public License as published by
%%%% the Free Software Foundation, either version 3 of the License, or
%%%% (at your option) any later version.
%%%%
%%%% lalily is distributed in the hope that it will be useful,
%%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%%% GNU General Public License for more details.
%%%%
%%%% You should have received a copy of the GNU General Public License
%%%% along with lalily.  If not, see <http://www.gnu.org/licenses/>.

\version "2.16.0"

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% book & test commands

#(define (add-sco-mup parser pre-markup score post-markup)
   (begin
    (cond ((markup? pre-markup)
           (begin
            (add-score parser (list pre-markup))
            (add-music parser #{ \noPageBreak #})))
      ((markup-list? pre-markup)
       (add-score parser pre-markup))
      )
    (add-score parser score)
    (cond ((markup? post-markup)
           (begin
            (add-music parser #{ \noPageBreak #})
            (add-score parser (list post-markup))))
      ((markup-list? post-markup)
       (add-score parser post-markup))
      )
    ))

% command to create one score based on the music of the current music folder with PDF and MIDI only if the containing file is compiled directly
\parserDefine lalilyTest
#(define-scheme-function (parser location)()
                (if ((get-registry-val '(lalily runtime test-predicate) lalily-test-location?) parser location)
                    (let ((score #{
                      \score {
                        \createScore #'()
                        \header { }
                      }
                            #})
                          (bookpart #{
                            \bookpart {
                              \paper {
                                $(get-music-folder-paper location)
                              }
                              \header { }
                            }
                            #})
                          (pre-markup (ly:assoc-get 'pre-markup (get-default-options (get-music-folder) location) #f #f))
                          (post-markup (ly:assoc-get 'post-markup (get-default-options (get-music-folder) location) #f #f))
                          (headers (assoc-get 'header (get-music-folder-options location) '()))
                          (copyright (get-registry-val '(lalily header copyright) #f))
                          (dolayout (not (eq? (get-registry-val '(lalily runtime test) #t) 'NoLayout)))
                          (domidi (not (eq? (get-registry-val '(lalily runtime test) #t) 'NoMidi)))
                          )
                      (if dolayout (ly:score-add-output-def! score (get-music-folder-layout location)))
                      (if domidi (ly:score-add-output-def! score (get-music-folder-midi location)))
                      (if (and copyright (not (assoc-get 'copyright headers)))
                          (set! headers (assoc-set! headers 'copyright copyright)))
                      (set-book-headers! bookpart headers)
                      (log-music-folder)
                      (ly:parser-define! parser '$current-bookpart bookpart)
                      (add-sco-mup parser pre-markup score post-markup)
                      (collect-bookpart-for-book parser bookpart)
                      (write-lalily-log-file parser)
                      )
                    )
                )

% create one score based on current music folder
\parserDefine lalilyScore
#(define-scheme-function (parser location options)(list?)
                 (let* ((domidi (ly:assoc-get 'midi options #f #f))
                        (extra (ly:assoc-get 'extra options '() #f))
                        (addopt (ly:assoc-get 'options options '() #f))
                        (score #{
                          \score {
                            \createScoreWithOptions #extra #addopt
                            \layout {
                              $(get-music-folder-layout location)
                            }
                            \header { }
                          }
                          #})
                        (headers (assoc-get 'header (get-music-folder-options location) '()))
                        (clear-headers (ly:assoc-get 'clear-headers options '() #f))
                        (pre-markup (ly:assoc-get 'pre-markup (get-default-options (get-music-folder) location) #f #f))
                        (post-markup (ly:assoc-get 'post-markup (get-default-options (get-music-folder) location) #f #f))
                        )
                   (set! headers (assoc-set-all! (map (lambda (p) `(,(car p) . #f)) clear-headers) headers))
                   (set-score-headers! score headers)
                   (log-music-folder)
                   (if domidi (ly:score-add-output-def! score (get-music-folder-midi location)))
                   (let ((title (get-music-folder-header-field 'toc-label)))
                     (if (not (markup? title))(set! title (get-music-folder-header-field 'title)))
                     (if (markup? title) (add-music parser #{ \tocPart $title #})))
                   (add-sco-mup parser pre-markup score post-markup)
                   ))

% create one bookpart containing named music folders
#(use-modules (srfi srfi-1))
\parserDefine lalilyBookpart 
#(define-scheme-function (parser location options)(list?)
                    (let* ((options (assoc-set-all! (get-music-folder-options location) options))
                           (cbp (ly:parser-lookup parser '$current-bookpart))
                           (keys (ly:assoc-get 'keys options (ly:assoc-get 'keys (get-music-folder-options location)) #f))
                           (mus (ly:assoc-get 'music options
                                  (let ((p (get-music-folder)))
                                    (map
                                     (lambda (k) (create-music-path #f (list k)))
                                     (if (and (list? keys)(> (length keys) 0))
                                         keys 
                                         (let* ((keys (get-music-keys p #f))
                                                (kformat (lambda (k) (if (number? k) (format "~5,'0d" k)(format "~A" k))))
                                                (sfun (lambda (k1 k2) (string<? (kformat k1) (kformat k2)))))
                                         (sort keys sfun)) )))
                                  #f))
                           (print-all-headers (ly:assoc-get 'print-all-headers options (or (list? keys)(list? mus)) #f))
                           (bookpart #{
                             \bookpart {
                               \paper {
                                 $(get-music-folder-paper location)
                                 print-all-headers = $print-all-headers
                               }
                               \header { }
                             }
                             #})
                           (doScore (ly:music-function-extract lalilyScore))
                           (doPart (ly:assoc-get 'toc-part options #f #f))
                           (headers (assoc-get 'header (get-music-folder-options location) '()))
                           )
                      (set-book-headers! bookpart headers)
                      (log-music-folder)
                      (ly:parser-define! parser '$current-bookpart bookpart)

                      (let ((title (get-music-folder-header-field 'toc-label)))
                        (if (not (markup? title))(set! title (get-music-folder-header-field 'title)))
                        (if (markup? title) (if doPart (add-music parser #{ \tocPart $title #})
                                                (add-music parser #{ \tocCollection $title #}))))

                      (if print-all-headers (set! options (assoc-set! options 'clear-headers headers)))
                      (for-each
                       (lambda (music)
                         (cond ((eq? (last music) 'PAGE-BREAK!)
                                (add-music parser #{ \pageBreak #}))
                           ((eq? (last music) 'PAGE-TURN!)
                            (add-music parser #{ \pageTurn #}))
                           ((eq? (last music) 'NO-PAGE-BREAK!)
                            (add-music parser #{ \noPageBreak #}))
                           ((eq? (last music) 'NO-PAGE-TURN!)
                            (add-music parser #{ \noPageTurn #}))
                           (else (let ((ctx (get-music-folder)))
                                   (set-music-folder! music)
                                   (doScore parser location options)
                                   (set-music-folder! ctx))))
                         ) mus)

                      (let ((book (ly:parser-lookup parser '$current-book)))
                        (if book
                            (ly:book-add-bookpart! book bookpart)
                            (collect-bookpart-for-book parser bookpart)))
                      (ly:parser-define! parser '$current-bookpart cbp)
                      #f
                      ))

% create one bookpart based on current music folder
\parserDefine lalilyBookpartScore 
#(define-scheme-function (parser location options)(list?)
                         (let* ((cbp (ly:parser-lookup parser '$current-bookpart))
                                (print-all-headers (ly:assoc-get 'print-all-headers options #f #f))
                                (domidi (ly:assoc-get 'midi options #f #f))
                                (score #{
                                  \score {
                                    \createScore #'()
                                    \layout {
                                      $(get-music-folder-layout location)
                                    }
                                    \header { }
                                  }
                                  #})
                                (bookpart #{
                                  \bookpart {
                                    \paper {
                                      $(get-music-folder-paper location)
                                    }
                                    \header { }
                                  }
                                  #})
                                (pre-markup (ly:assoc-get 'pre-markup (get-default-options (get-music-folder) location) #f #f))
                                (post-markup (ly:assoc-get 'post-markup (get-default-options (get-music-folder) location) #f #f))
                                (doPart (ly:assoc-get 'toc-part options #f #f))
                                )
                           (set-book-headers! bookpart (assoc-get 'header (get-music-folder-options location) '()))
                           (log-music-folder)
                           (ly:parser-define! parser '$current-bookpart bookpart)
                           (if domidi (ly:score-add-output-def! score (get-music-folder-midi location)))

                           (let ((title (get-music-folder-header-field 'toc-label)))
                             (if (not (markup? title))(set! title (get-music-folder-header-field 'title)))
                             (if (markup? title) (if doPart (add-music parser #{ \tocPart $title #})
                                                     (add-music parser #{ \tocCollection $title #}))))

                           (add-sco-mup parser pre-markup score post-markup)

                           (let ((book (ly:parser-lookup parser '$current-book)))
                             (if book
                                 (ly:book-add-bookpart! book bookpart)
                                 (collect-bookpart-for-book parser bookpart)))
                           (ly:parser-define! parser '$current-bookpart cbp)
                           #f
                           ))

% test versions of above commands, executed only, if test predicate is met
% default: name of location equals name of parser output
\parserDefine lalilyTestScore 
#(define-scheme-function (parser location options)(list?)
                     (if ((get-registry-val '(lalily runtime test-predicate) lalily-test-location?) parser location)
                         (begin
                          ((ly:music-function-extract lalilyScore) parser location options)
                          (write-lalily-log-file parser)
                          ))
                     )
\parserDefine lalilyTestBookpart 
#(define-scheme-function (parser location options)(list?)
                        (if ((get-registry-val '(lalily runtime test-predicate) lalily-test-location?) parser location)
                            (begin
                             ((ly:music-function-extract lalilyBookpart) parser location options)
                             (write-lalily-log-file parser)
                             ))
                        )
\parserDefine lalilyTestBookpartScore 
#(define-scheme-function (parser location options)(list?)
                             (if ((get-registry-val '(lalily runtime test-predicate) lalily-test-location?) parser location)
                                 (begin
                                  ((ly:music-function-extract lalilyBookpartScore) parser location options)
                                  (write-lalily-log-file parser)
                                  ))
                             )

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% write log file
\parserDefine lalilyWriteLog 
#(define-scheme-function (parser location)()
                    (write-lalily-log-file parser))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% write annotations
#(use-modules (oop goops))

#(define-class <annotation> ()
   (piece #:init-value "" #:accessor piece #:setter set-piece! #:init-keyword #:piece)
   (category #:init-value 'TODO #:accessor category #:setter set-category! #:init-keyword #:category)
   (title #:init-value "???" #:accessor title #:setter set-title! #:init-keyword #:title)
   (annotation #:init-value "" #:accessor annotation #:setter set-annotation! #:init-keyword #:annotation)
   (page-ref #:init-value #f #:accessor page-ref #:setter set-page-ref! #:init-keyword #:page-ref)
   (measure #:init-value 0 #:accessor measure #:setter set-measure! #:init-keyword #:measure)
   (position #:init-value (ly:make-moment 0 0) #:accessor position #:setter set-position! #:init-keyword #:position)
   (moment #:init-value (ly:make-moment 0 0) #:accessor moment #:setter set-moment! #:init-keyword #:moment)
   )
#(define-method (anno-pos (a <annotation>))
   (let ((mom (position a)))
     (format "~A, ~A/~A" (measure a) (+ 1 (ly:moment-main-numerator mom)) (ly:moment-main-denominator mom))
     ))
#(define-method (display (a <annotation>) port)
   (format #t "~A: ~A" (anno-pos a) (markup->string (title a)) port))

#(define-public (make-anno cat title text . opts)
   (let ((anno (make <annotation> #:category cat #:title title #:annotation text))
         (hasm #f))
     (for-each (lambda (p) (cond ((symbol? p) (set-page-ref! anno p))
                             ((integer? p) (begin (set-measure! anno p)(set! hasm #t)))
                             ((ly:moment? p) (if hasm (set-position! anno p) (set-moment! p))))
                 ) opts)
     anno))
#(define-public (annotation? a) (is-a? a <annotation>))
#(define-public (annotation<? a1 a2)
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


\parserDefine text 
#(define-music-function (parser location tweaks opts txt)((list? '()) (list? '()) markup?)
          (let ((m (make-music 'TextScriptEvent 'text txt)))
            (for-each (lambda (p)
                        (if (pair? p)
                            (let ((key (car p))
                                  (val (cdr p)))
                              (cond ((eq? key 'style)
                                     (ly:music-set-property! m 'text (markup #:style val txt)))
                                (else (ly:music-set-property! m key val)))))) opts)
            (if (> (length tweaks) 0) (ly:music-set-property! m 'tweaks tweaks))
            m))
\parserDefine todo 
#(define-music-function (parser location tweaks opts title txt)((list? '()) (list? '()) markup? markup?)
          (let ((txta (ly:music-function-extract text))
                (defopts `((style . TBD)(annotation . ,(make-anno 'TODO title txt))())))
            (txta parser location tweaks (assoc-set-all! defopts opts) title)
            ))


#(define-public (annotations pc) '())
\parserDefine annocollect
#(let ((msgs '())
       (instance 1)
       (file-written #f))
   (set! annotations
         (lambda (pc) (sort (if pc (filter (lambda (a) (string=? pc (piece a))) msgs) msgs) annotation<?)))
   (lambda (context)
     (let* ((outname (ly:parser-output-name (get-registry-val lalily:registry-parser)))
            (pc (format "~3,'0d" instance))
            (printmsgs (lambda()
                         (let ((todofile (format "~A-todo.log" outname pc))
                               (annos (annotations pc)))
                           (if (> (length annos) 0)
                               (let ((with-output (if file-written with-append-file (begin (ly:message "writing ~A" todofile) with-output-to-file))))
                                 (set! file-written #t)
                                 (with-output todofile
                                   (lambda ()
                                     (let ((section #f))
                                       (format #t "~A\n---\n" (if (string? pc) pc (glue-list (get-music-folder) "/")))
                                       (for-each (lambda (a)
                                                   (if (not (eq? section (category a)))
                                                       (begin (set! section (category a))(display section)(newline)(newline)))
                                                   (display "Takt ")(display a)(newline)
                                                   (display (markup->string (annotation a)))(newline)(newline))
                                         annos
                                         ))))))))))
       (set! instance (1+ instance))
       (make-engraver
        (listeners ((text-script-event engraver event)
                    (let* ((origin (ly:event-property event 'origin))
                           (m (ly:event-property event 'music-cause))
                           (todolog (if (ly:music? m) (ly:music-property m 'annotation #f) #f))
                           (msg (if todolog (markup->string (ly:music-property m 'todomsg "No Message"))))
                           (curpos (ly:context-current-moment context))
                           (takt (ly:context-property context 'currentBarNumber))
                           (mpos (ly:context-property context 'measurePosition)))
                      (if (annotation? todolog)
                          (let ((c 0))
                            (set-piece! todolog pc)
                            (set-measure! todolog takt)
                            (set-position! todolog mpos)
                            (set-moment! todolog curpos)
                            (set! msgs (append msgs (list todolog)))
                            ))
                      )))
        ((finalize trans) (printmsgs))
        ))))

\parserDefine anntitle \markup { \override #'(baseline-skip . 0) \left-column { \huge \bold \fromproperty #'anno:piece \hrule \vspace #0.5 } }
\parserDefine annentry \markup {
  \left-column {
    \override #'(hilite-color . (0.95 0.95 0.95)) \hilite \fill-line { \line { \fromproperty #'anno:index - Takt \fromproperty #'anno:position : \fromproperty #'anno:title } \null }
    \justify { \fromproperty #'anno:text }
    \vspace #0.5
} }
#(define-markup-list-command (annolist layout props)()
   (let ((mups (list (interpret-markup layout props #{ \markup \fill-line { \huge \bold "Anmerkungen" } #})))
         (pc #f)
         (ac #f)
         (c 0))
     (for-each (lambda (a)
                 (set! c (1+ c))
                 (if (not (eq? pc (piece a)))
                     (begin
                      (set! pc (piece a))
                      (set! ac #f)
                      (if (markup? pc) (append! mups (list (interpret-markup layout (cons `((anno:piece . ,pc)) props) anntitle))) )))
                 (if (not (eq? ac (category a)))
                     (begin
                      (set! ac (category a))
                      (append! mups (list (interpret-markup layout props #{ \markup \fill-line { $(format "~A" ac) \general-align #Y #CENTER \vspace #2 } #})))
                      ))
                 (append! mups (list
                                (interpret-markup layout (cons `((anno:index . ,(format "~2,'0d" c))
                                                                 (anno:position . ,(anno-pos a))
                                                                 (anno:title . ,(title a))
                                                                 (anno:text . ,(annotation a))) props) annentry))))
       (annotations #f))
     mups
     ))

\parserDefine annofile

\layout {
  \context {
    \Score
    \consists \annocollect
    \consists \editionEngraver ##f
  }
  \context {
    \Voice
    \consists \editionEngraver ##f
  }
}
