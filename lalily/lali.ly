%%%% This file is part of lalily, an extension to lilypond <http://www.lilypond.org/>.
%%%%
%%%% Copyright (C) 2011--2016 Jan-Peter Voigt <jp.voigt@gmx.de>
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

\version "2.19.32"
%%% this file is included by lalily.ily and won't compile itself

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% book & test commands

#(define (add-sco-mup pre-markup score post-markup)
   (begin
    (cond ((markup? pre-markup)
           (begin
            (add-score (list pre-markup))
            (add-music #{ \noPageBreak #})))
      ((markup-list? pre-markup)
       (add-score pre-markup))
      )
    (add-score score)
    (cond ((markup? post-markup)
           (begin
            (add-music #{ \noPageBreak #})
            (add-score (list post-markup))))
      ((markup-list? post-markup)
       (add-score post-markup))
      )
    ))

% command to create one score based on the current "music folder"
\parserDefine lalilyCreate
#(define-void-function ()()
   (let ((score #{
     \score {
       \createScore #'()
       \header { }
     }
           #})
         (bookpart #{
           \bookpart {
             \paper {
               $(get-music-folder-paper)
             }
             \header { }
           }
           #})
         (pre-markup (ly:assoc-get 'pre-markup (get-default-options (get-music-folder)) #f #f))
         (post-markup (ly:assoc-get 'post-markup (get-default-options (get-music-folder)) #f #f))
         (headers (assoc-get 'header (get-music-folder-options) '()))
         (copyright (get-registry-val '(lalily header copyright) #f))
         (dolayout (not (eq? (get-registry-val lalily:create #t) 'NoLayout)))
         (domidi (not (eq? (get-registry-val lalily:create #t) 'NoMidi)))
         )
     (if dolayout
         (begin
          (ly:score-add-output-def! score #{
            \layout {
              $(get-music-folder-layout)
              \context {
                \Score
                \consists \editionEngraver ##f
                \consists \annoCollect
              }
              \context {
                \Voice
                \consists \editionEngraver ##f
              }
            }
            #})
          ))
     (if domidi (ly:score-add-output-def! score (get-music-folder-midi)))
     (if (and copyright (not (assoc-get 'copyright headers)))
         (set! headers (assoc-set! headers 'copyright copyright)))
     (set-book-headers! bookpart headers)
     (log-music-folder)
     (ly:parser-define! '$current-bookpart bookpart)
     (add-sco-mup pre-markup score post-markup)
     (collect-bookpart-for-book bookpart)
     (write-lalily-log-file)
     ))


% command to create one score based on the music of the current music folder with PDF and MIDI only if the containing file is compiled directly
\parserDefine lalilyTest
#(define-void-function ()()
   (if ((get-registry-val lalily:test-predicate lalily-test-location?) (*parser*) (*location*))
       (lalilyCreate)
       ))

% create one score based on current music folder
\parserDefine lalilyScore
#(define-void-function (options)(list?)
   (let* ((domidi (ly:assoc-get 'midi options #f #f))
          (extra (ly:assoc-get 'extra options '() #f))
          (addopt (ly:assoc-get 'options options '() #f))
          (score #{
            \score {
              \createScoreWithOptions #extra #addopt
              \layout {
                $(get-music-folder-layout)
                \context {
                  \Score
                  \consists \editionEngraver #(get-music-folder)
                  \consists \annoCollect
                }
                \context {
                  \Voice
                  \consists \editionEngraver ##f
                }
              }
              \header { }
            }
            #})
          (headers (assoc-get 'header (get-music-folder-options) '()))
          (clear-headers (ly:assoc-get 'clear-headers options '() #f))
          (pre-markup (ly:assoc-get 'pre-markup (get-default-options (get-music-folder)) #f #f))
          (post-markup (ly:assoc-get 'post-markup (get-default-options (get-music-folder)) #f #f))
          )
     (set! headers (assoc-set-all! (map (lambda (p) `(,(car p) . #f)) clear-headers) headers))
     (set-score-headers! score headers)
     (log-music-folder)
     (if domidi (ly:score-add-output-def! score (get-music-folder-midi)))
     (let ((title (get-music-folder-header-field 'toc-label)))
       (if (not (markup? title))(set! title (get-music-folder-header-field 'title)))
       (if (markup? title) (add-music #{ \tocPart $title #})))
     (add-sco-mup pre-markup score post-markup)
     ))

% create one bookpart containing named music folders
#(use-modules (srfi srfi-1))
\parserDefine lalilyBookpart
#(define-void-function (options)(list?)
   (let* ((options (assoc-set-all! (get-music-folder-options) options))
          (cbp (ly:parser-lookup '$current-bookpart))
          (keys (ly:assoc-get 'keys options (ly:assoc-get 'keys (get-music-folder-options)) #f))
          (mus (ly:assoc-get 'music options
                 (let ((p (get-music-folder)))
                   (map
                    (lambda (k) (create-music-path #f (list k)))
                    (if (and (list? keys)(> (length keys) 0))
                        keys
                        (let* ((keys (get-music-keys p))
                               (kformat (lambda (k) (if (number? k) (format "~5,'0d" k)(format "~A" k))))
                               (sfun (lambda (k1 k2) (string<? (kformat k1) (kformat k2)))))
                          (sort keys sfun)) )))
                 #f))
          (print-all-headers (ly:assoc-get 'print-all-headers options (or (list? keys)(list? mus)) #f))
          (bookpart #{
            \bookpart {
              \paper {
                $(get-music-folder-paper)
                print-all-headers = $print-all-headers
              }
              \header { }
            }
            #})
          (doScore (ly:music-function-extract lalilyScore))
          (doPart (ly:assoc-get 'toc-part options #f #f))
          (headers (assoc-get 'header (get-music-folder-options) '()))
          )
     (set-book-headers! bookpart headers)
     (log-music-folder)
     (ly:parser-define! '$current-bookpart bookpart)

     (let ((title (get-music-folder-header-field 'toc-label)))
       (if (not (markup? title))(set! title (get-music-folder-header-field 'title)))
       (if (markup? title) (if doPart (add-music #{ \tocPart $title #})
                               (add-music #{ \tocCollection $title #}))))

     (if print-all-headers (set! options (assoc-set! options 'clear-headers headers)))
     (for-each
      (lambda (music)
        (cond ((eq? (last music) 'PAGE-BREAK!)
               (add-music #{ \pageBreak #}))
          ((eq? (last music) 'PAGE-TURN!)
           (add-music #{ \pageTurn #}))
          ((eq? (last music) 'NO-PAGE-BREAK!)
           (add-music #{ \noPageBreak #}))
          ((eq? (last music) 'NO-PAGE-TURN!)
           (add-music #{ \noPageTurn #}))
          ((and (list? (last music))(eq? (car (last music)) 'MARKUP))
           (let* ((ctx (get-music-folder))
                  (alist (cdr (last music)))
                  (stl (ly:assoc-get 'style alist #f #t))
                  (mkey (ly:assoc-get 'key alist #f #t))
                  (music (append (reverse (cdr (reverse music))) (list mkey)))
                  (dopre (ly:assoc-get 'pre-markup alist #f #f))
                  (dopost (ly:assoc-get 'post-markup alist #f #f))
                  (opts (get-default-options music))
                  (header (ly:assoc-get 'header opts))
                  (text (ly:assoc-get 'text alist "" #f))
                  (pre-markup (if dopre (ly:assoc-get 'pre-markup opts #f #f) #f))
                  (post-markup (if dopre (ly:assoc-get 'post-markup opts #f #f) #f))
                  )
             (cond ((markup? pre-markup)
                    (begin
                     (add-score (list pre-markup))
                     (add-music #{ \noPageBreak #})))
               ((markup-list? pre-markup)
                (add-score pre-markup))
               )
             (add-score #{
               \markuplist {
                 \with-props #(map (lambda (p)
                                     (cons (string->symbol (format "header:~A" (car p)))
                                       (cdr p))) header)
                 \style #stl $text
               } #})
             (cond ((markup? post-markup)
                    (begin
                     (add-music #{ \noPageBreak #})
                     (add-score (list post-markup))))
               ((markup-list? post-markup)
                (add-score post-markup))
               )
             ))
          ((and (list? (last music))(eq? (car (last music)) 'MARKUPLIST))
           (let* ((ctx (get-music-folder))
                  (muslist (cdr (last music)))
                  (muplist (map
                            (lambda (music)
                              (let* (
                                      (alist (cdr (last music)))
                                      (stl (ly:assoc-get 'style alist #f #t))
                                      (mkey (ly:assoc-get 'key alist #f #t))
                                      (music (append (reverse (cdr (reverse music))) (list mkey)))
                                      (dopre (ly:assoc-get 'pre-markup alist #f #f))
                                      (dopost (ly:assoc-get 'post-markup alist #f #f))
                                      (opts (get-default-options music))
                                      (header (ly:assoc-get 'header opts))
                                      (text (ly:assoc-get 'text alist "" #f))
                                      (pre-markup (if dopre (ly:assoc-get 'pre-markup opts #f #f) #f))
                                      (post-markup (if dopre (ly:assoc-get 'post-markup opts #f #f) #f))
                                      )
                                (if (list? header)
                                    #{
                                      \markup {
                                        \with-props #(map (lambda (p)
                                                            (cons (string->symbol (format "header:~A" (car p)))
                                                              (cdr p))) header)
                                        \style #stl $text
                                      }
                                    #}
                                    #{ \markup \style #stl $text #})
                                )) muslist)))
             (add-score muplist)
             ))

          (else (let ((ctx (get-music-folder)))
                  (set-music-folder! music)
                  (doScore options)
                  (set-music-folder! ctx))))
        ) mus)

     (let ((book (ly:parser-lookup '$current-book)))
       (if book
           (ly:book-add-bookpart! book bookpart)
           (collect-bookpart-for-book bookpart)))
     (ly:parser-define! '$current-bookpart cbp)
     ))

% create one bookpart based on current music folder
\parserDefine lalilyBookpartScore
#(define-void-function (options)(list?)
   (let* ((cbp (ly:parser-lookup '$current-bookpart))
          (print-all-headers (ly:assoc-get 'print-all-headers options #f #f))
          (domidi (ly:assoc-get 'midi options #f #f))
          (score #{
            \score {
              \createScore #'()
              \layout {
                $(get-music-folder-layout)
                \context {
                  \Score
                  \consists \editionEngraver #(get-music-folder)
                  \consists \annoCollect
                }
                \context {
                  \Voice
                  \consists \editionEngraver ##f
                }
              }
              \header { }
            }
            #})
          (bookpart #{
            \bookpart {
              \paper {
                $(get-music-folder-paper)
              }
              \header { }
            }
            #})
          (pre-markup (ly:assoc-get 'pre-markup (get-default-options (get-music-folder)) #f #f))
          (post-markup (ly:assoc-get 'post-markup (get-default-options (get-music-folder)) #f #f))
          (doPart (ly:assoc-get 'toc-part options #f #f))
          )
     (set-book-headers! bookpart (assoc-get 'header (get-music-folder-options) '()))
     (log-music-folder)
     (ly:parser-define! '$current-bookpart bookpart)
     (if domidi (ly:score-add-output-def! score (get-music-folder-midi)))

     (let ((title (get-music-folder-header-field 'toc-label)))
       (if (not (markup? title))(set! title (get-music-folder-header-field 'title)))
       (if (markup? title) (if doPart (add-music #{ \tocPart $title #})
                               (add-music #{ \tocCollection $title #}))))

     (add-sco-mup pre-markup score post-markup)

     (let ((book (ly:parser-lookup '$current-book)))
       (if book
           (ly:book-add-bookpart! book bookpart)
           (collect-bookpart-for-book bookpart)))
     (ly:parser-define! '$current-bookpart cbp)
     #f
     ))

\parserDefine lalilyBookparts
#(define-void-function (options)(list?)
   (let* ((keys (ly:assoc-get 'keys options (ly:assoc-get 'keys (get-music-folder-options)) #f))
          (mus (ly:assoc-get 'music options
                 (let ((p (get-music-folder)))
                   (map
                    (lambda (k) (create-music-path #f (list k)))
                    (if (and (list? keys)(> (length keys) 0))
                        keys
                        (let* ((keys (get-music-keys p))
                               (kformat (lambda (k) (if (number? k) (format "~5,'0d" k)(format "~A" k))))
                               (sfun (lambda (k1 k2) (string<? (kformat k1) (kformat k2)))))
                          (sort keys sfun)) )))
                 #f))
          )
     (for-each (lambda (music)
                 (let ((ctx (get-music-folder)))
                   (set-music-folder! music)
                   (lalilyBookpartScore options)
                   (set-music-folder! ctx)
                   )) mus)
     ))

% test versions of above commands, executed only, if test predicate is met
% default: name of location equals name of parser output
\parserDefine lalilyTestScore
#(define-void-function (options)(list?)
   (if ((get-registry-val lalily:test-predicate lalily-test-location?) (*parser*) (*location*))
       (begin
        ((ly:music-function-extract lalilyScore)  options)
        (write-lalily-log-file)
        ))
   )
\parserDefine lalilyTestBookpart
#(define-void-function (options)(list?)
   (if ((get-registry-val lalily:test-predicate lalily-test-location?) (*parser*) (*location*))
       (begin
        ((ly:music-function-extract lalilyBookpart)  options)
        (write-lalily-log-file)
        ))
   )
\parserDefine lalilyTestBookpartScore
#(define-void-function (options)(list?)
   (if ((get-registry-val lalily:test-predicate lalily-test-location?) (*parser*) (*location*))
       (begin
        ((ly:music-function-extract lalilyBookpartScore)  options)
        (write-lalily-log-file)
        ))
   )
\parserDefine lalilyTestBookparts
#(define-void-function (options)(list?)
   (if ((get-registry-val lalily:test-predicate lalily-test-location?) (*parser*) (*location*))
       (begin
        ((ly:music-function-extract lalilyBookparts)  options)
        (write-lalily-log-file)
        ))
   )

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% write log file
\parserDefine lalilyWriteLog
#(define-void-function ()()
   (write-lalily-log-file))
