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

% This file is included conditionaly by lalily.ly
\version "2.16.0"

#(define-public lalily-startup #t)

#(use-modules (ice-9 regex))
#(define-public (listcwd) '())
#(define-public (absolutePath? path) #f)
#(let* ((os (getenv "OS"))
        (isWindows (if (and (string? os) (regexp-exec (make-regexp ".*Windows.*" regexp/icase) os)) #t #f))
        (wrx (if isWindows (make-regexp "^[a-z]:$" regexp/icase) #f)))
   (set! listcwd (lambda () (string-split (getcwd)(if isWindows #\\ #\/))))
   (set! absolutePath?
         (lambda (path)
           (if isWindows
               (if (and (> (length path) 0) (regexp-exec wrx (car path))) #t #f)
               (and (> (length path) 0) (= 0 (string-length (car path))))
               )))
   )

% add guile load path relative to this file
#(define-public addGuilePath
   (define-music-function (parser location folder)(string?)
     (let* ((loclist (string-split (car (ly:input-file-line-char-column location)) #\/))
            (path-extra (let ((pl (if (absolutePath? loclist)
                                      loclist (append (listcwd) loclist))))
                          (string-join (reverse (cdr (reverse pl))) "/" 'infix)))
            (normalize-list (lambda (path)
                              (let ((ret '()))
                                (for-each (lambda (e)
                                            (set! ret (cond ((equal? e "..")(if (> (length ret) 1) (cdr ret) '()))
                                                        ((equal? e ".") ret)
                                                        (else `(,e ,@ret))))) path)
                                (reverse ret))))
            (normalize-path (lambda (s) (string-join (normalize-list (string-split s #\/)) "/" 'infix))))
       (set! path-extra (if (string? path-extra) (string-append (if (eq? (string-ref path-extra 0) #\.) path-extra (normalize-path path-extra)) "/" folder) folder))
       (if (not (member path-extra %load-path)) (set! %load-path `(,path-extra ,@%load-path)))
       ;(display %load-path)
       (make-music 'SequentialMusic 'void #t))))
% add folder "lalily" next to this file to guile load path
\addGuilePath "lalily"

% init scheme modules introduced by lalily
#(if (not (defined? 'lalily:init))(load-from-path "lalily/init.scm"))

% "re-import" previously defined values for session based runs like in lilypond-book
#(set-registry-val lalily:registry-parser parser)
#(let ((defs (get-registry-val lalily:registry-parser-defs '())))
   (for-each (lambda (p)(ly:parser-define! parser (car p )(cdr p))) defs))


% include (once) from lalily folder
#(define-public lalilyInclude (define-music-function (parser location file)(string?)
                                (ly:input-message location "lalily include not initialized!")
                                (make-music 'SequentialMusic 'void #t)))
#(define-public lalilyIncludeOnce (define-music-function (parser location file)(string?)
                                    (ly:input-message location "lalily include not initialized!")
                                    (make-music 'SequentialMusic 'void #t)))
#(define-public lalilyIncludeScheme (define-music-function (parser location file)(string?)
                                      (ly:input-message location "lalily include not initialized!")
                                      (make-music 'SequentialMusic 'void #t)))

\execMusic #(lambda (parser location)
              (begin
               (let* ((path-extra (location-extract-path location)))
                 (set! path-extra (if path-extra (normalize-path-string path-extra) ""))
                 (set-registry-val '(lalily runtime path)
                   (string-append path-extra
                     (if (and (> (string-length path-extra) 0)
                              (not (eq? (string-ref path-extra (- (string-length path-extra) 1)) #\/)))
                         "/" "") "lalily/"))
                 ;(ly:message "lalily path: ~A" (get-registry-val '(lalily runtime path)))
                 (set! lalilyInclude (define-music-function (parser location file)(string?)
                                       (let ((file-path (string-append path-extra "lalily/" file)))
                                         (set! file-path (normalize-path-string file-path))
                                         (if (file-exists? file-path)
                                             (la:parser-include-file parser file-path #f)
                                             (ly:input-message location "WARNING: file '~A' not found" file-path))
                                         (make-music 'SequentialMusic 'void #t)
                                         )))
                 (set! lalilyIncludeOnce (define-music-function (parser location file)(string?)
                                           (let ((file-path (string-append path-extra "lalily/" file)))
                                             (set! file-path (normalize-path-string file-path))
                                             (if (file-exists? file-path)
                                                 (la:parser-include-file parser file-path #t)
                                                 (ly:input-message location "WARNING: file '~A' not found" file-path))
                                             (make-music 'SequentialMusic 'void #t)
                                             )))
                 (set! lalilyIncludeScheme (define-music-function (parser location file)(string?)
                                             (let ((file-path (string-append path-extra "lalily/" file)))
                                               (set! file-path (normalize-path-string file-path))
                                               (if (file-exists? file-path)
                                                   (begin
                                                    (if (lalily:verbose) (ly:message "loading '~A' ..." file-path))
                                                    (load-from-path file-path)
                                                    ))
                                               (make-music 'SequentialMusic 'void #t)
                                               )))
                 )
               (make-music 'SequentialMusic 'void #t)))

%load custom config
\lalilyIncludeScheme "../lalily-extensions/config.scm"
\lalilyIncludeScheme "../../lalily-extensions/config.scm"

\lalilyInclude "lali.ly"

% look for lalily paper/layout/midi
\includeOncePattern "lalily" "^paper(\..*)?\.ly$" % once?
\includeOncePattern "lalily" "^layout(\..*)?\.ly$" % once?
\includeOncePattern "lalily" "^midi(\..*)?\.ly$" % once?
% look for extensions
\includeOncePattern "lalily/extensions" "^.*\.ly$" % once?
\includeOncePattern "lalily-extensions" "^.*\.ly$" % once?
\includeOncePattern "../lalily-extensions" "^.*\.ly$" % once?

% look for local paper/layout/midi
\includeOnceIfExists "lalily-paper.ly" % once?
\includeOnceIfExists "lalily-layout.ly" % once?
\includeOnceIfExists "lalily-midi.ly" % once?

% look for local config
\includeOnceIfExists "lalily-config.ly" %once?
#(if (and (defined? 'lalilyConfig)(list? lalilyConfig))
     (for-each (lambda (p)
                 (if (and (pair? p)(list? (car p)))
                     (begin
                      (ly:message "config ~A=~A" (car p )(cdr p))
                      (set-registry-val (car p) (cdr p))
                      ))
                 ) lalilyConfig))

% look for lalily templates
\includeOncePattern "lalily" "^templates(\..*)?\.ly$" % once?
% look for local templates
\includeOnceIfExists "lalily-templates.ly" % once?

% look for editions
\includeOnceIfExists "lalily-edition.ly" % once?

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Std layout

#(define (do-layout parser)
   (and
    (not (eq? #t (ly:parser-lookup parser 'lalilyNoOutputDef)))
    (not (defined? 'lalily-no-output-def))
    (not (ly:get-option 'lalily-no-output-def))
    ))

% if allowed, set global paper and layout with default
\includeRelIf "lalily/output-default.ly"
#(lambda (parser location)
   (let ((ret (do-layout parser)))
     (if (and (not ret) (lalily:verbose))
         (ly:message "no lalily output-defs!"))
     ret))

#(let ((gss (get-registry-val '(lalily paper global-staff-size))))
   (if (and (do-layout parser) (number? gss)) (set-global-staff-size gss)))

