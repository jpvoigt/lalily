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

\version "2.17.29"
%%% this file is included by lalily.ily and won't compile itself

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

% command to create one score based on the current "music folder"
\parserDefine lalilyCreate
#(define-void-function (parser location)()
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
         (dolayout (not (eq? (get-registry-val lalily:create #t) 'NoLayout)))
         (domidi (not (eq? (get-registry-val lalily:create #t) 'NoMidi)))
         )
     (if dolayout
         (begin
          (ly:score-add-output-def! score #{
            \layout {
              $(get-music-folder-layout location)
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
     (if domidi (ly:score-add-output-def! score (get-music-folder-midi location)))
     (if (and copyright (not (assoc-get 'copyright headers)))
         (set! headers (assoc-set! headers 'copyright copyright)))
     (set-book-headers! bookpart headers)
     (log-music-folder)
     (ly:parser-define! parser '$current-bookpart bookpart)
     (add-sco-mup parser pre-markup score post-markup)
     (collect-bookpart-for-book parser bookpart)
     (write-lalily-log-file parser)
     ))


% command to create one score based on the music of the current music folder with PDF and MIDI only if the containing file is compiled directly
\parserDefine lalilyTest
#(define-void-function (parser location)()
   (if ((get-registry-val lalily:test-predicate lalily-test-location?) parser location)
       ((ly:music-function-extract lalilyCreate) parser location)
       ))

% create one score based on current music folder
\parserDefine lalilyScore
#(define-void-function (parser location options)(list?)
   (let* ((domidi (ly:assoc-get 'midi options #f #f))
          (extra (ly:assoc-get 'extra options '() #f))
          (addopt (ly:assoc-get 'options options '() #f))
          (score #{
            \score {
              \createScoreWithOptions #extra #addopt
              \layout {
                $(get-music-folder-layout location)
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
#(define-void-function (parser location options)(list?)
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
     ))

% create one bookpart based on current music folder
\parserDefine lalilyBookpartScore
#(define-void-function (parser location options)(list?)
   (let* ((cbp (ly:parser-lookup parser '$current-bookpart))
          (print-all-headers (ly:assoc-get 'print-all-headers options #f #f))
          (domidi (ly:assoc-get 'midi options #f #f))
          (score #{
            \score {
              \createScore #'()
              \layout {
                $(get-music-folder-layout location)
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

\parserDefine lalilyBookparts
#(define-void-function (parser location options)(list?)
   (let* ((lbpf (ly:music-function-extract lalilyBookpartScore))
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
          )
     (for-each (lambda (music)
                 (let ((ctx (get-music-folder)))
                   (set-music-folder! music)
                   (lbpf parser location options)
                   (set-music-folder! ctx)
                   )) mus)
     ))

% test versions of above commands, executed only, if test predicate is met
% default: name of location equals name of parser output
\parserDefine lalilyTestScore
#(define-void-function (parser location options)(list?)
   (if ((get-registry-val lalily:test-predicate lalily-test-location?) parser location)
       (begin
        ((ly:music-function-extract lalilyScore) parser location options)
        (write-lalily-log-file parser)
        ))
   )
\parserDefine lalilyTestBookpart
#(define-void-function (parser location options)(list?)
   (if ((get-registry-val lalily:test-predicate lalily-test-location?) parser location)
       (begin
        ((ly:music-function-extract lalilyBookpart) parser location options)
        (write-lalily-log-file parser)
        ))
   )
\parserDefine lalilyTestBookpartScore
#(define-void-function (parser location options)(list?)
   (if ((get-registry-val lalily:test-predicate lalily-test-location?) parser location)
       (begin
        ((ly:music-function-extract lalilyBookpartScore) parser location options)
        (write-lalily-log-file parser)
        ))
   )
\parserDefine lalilyTestBookparts
#(define-void-function (parser location options)(list?)
   (if ((get-registry-val lalily:test-predicate lalily-test-location?) parser location)
       (begin
        ((ly:music-function-extract lalilyBookparts) parser location options)
        (write-lalily-log-file parser)
        ))
   )

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% write log file
\parserDefine lalilyWriteLog
#(define-void-function (parser location)()
   (write-lalily-log-file parser))

