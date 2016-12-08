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

% This file is included conditionaly by lalily.ly
\version "2.19.32"

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
   (define-music-function (folder)(string?)
     (let* ((loclist (string-split (car (ly:input-file-line-char-column (*location*))) #\/))
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
\addGuilePath "."

% init scheme modules introduced by lalily
#(if (not (defined? 'lalily:init))(load-from-path "lalily/init.scm"))

% "re-import" previously defined values for session based runs like in lilypond-book
#(set-registry-val lalily:registry-parser (*parser*))
#(let ((defs (get-registry-val lalily:registry-parser-defs '())))
   (for-each (lambda (p)(ly:parser-define! (car p )(cdr p))) defs))

% look for local config
applyConfig =
#(define-void-function (CONFIG config)(symbol? procedure?)
   (if (and (defined? CONFIG)(list? (eval lalilyConfig (current-module))))
       (let ((lc (eval lalilyConfig (current-module))))
         (define (walk-tree tree path)
           (for-each
            (lambda (p)
              (if (pair? p)
                  (let ((ckey (car p))
                        (cval (cdr p)))
                    (if (and (= 0 (length path))(list? ckey))
                        (begin
                         (ly:warning "deprecated config: ~A" ckey)
                         (config ckey cval)
                         )
                        (if (and (list? cval)(every pair? cval)(> (length cval) 0))
                            (walk-tree cval `(,@path ,ckey))
                            (config `(,@path ,ckey) cval)
                            )
                        ))
                  ))
            tree))
         (walk-tree lc '())
         )))
registerConfig = #(lambda (ckey cval)
                    (ly:message "config ~A=~A" ckey cval)
                    (set-registry-val ckey cval))
\applyConfig lalilyConfig #registerConfig


% include (once) from lalily folder
#(define-public lalilyInclude (define-void-function (file)(string?)
                                (ly:input-message (*location*) "lalily include not initialized!")
                                ))
#(define-public lalilyIncludeOnce (define-void-function (file)(string?)
                                    (ly:input-message (*location*) "lalily include not initialized!")
                                    ))
#(define-public lalilyIncludeScheme (define-void-function (file)(string?)
                                      (ly:input-message (*location*) "lalily include not initialized!")
                                      ))

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
                 (set! lalilyInclude (define-void-function (file)(string?)
                                       (let ((file-path (string-append path-extra "" file)))
                                         (set! file-path (normalize-path-string file-path))
                                         (if (file-exists? file-path)
                                             (la:parser-include-file file-path #f)
                                             (ly:input-message (*location*) "WARNING: file '~A' not found" file-path))
                                         )))
                 (set! lalilyIncludeOnce (define-void-function (file)(string?)
                                           (let ((file-path (string-append path-extra "lalily/" file)))
                                             (set! file-path (normalize-path-string file-path))
                                             (if (file-exists? file-path)
                                                 (la:parser-include-file (*parser*) file-path #t)
                                                 (ly:input-message (*location*) "WARNING: file '~A' not found" file-path))
                                             )))
                 (set! lalilyIncludeScheme (define-void-function (file)(string?)
                                             (let ((file-path (string-append path-extra "lalily/" file)))
                                               (set! file-path (normalize-path-string file-path))
                                               (if (file-exists? file-path)
                                                   (begin
                                                    (if (lalily:verbose) (ly:message "loading '~A' ..." file-path))
                                                    (load-from-path file-path)
                                                    ))
                                               )))
                 )
               (make-music 'SequentialMusic 'void #t)))

%load custom config
\lalilyIncludeScheme "../lalily-extensions/config.scm"
\lalilyIncludeScheme "../../lalily-extensions/config.scm"

\lalilyInclude "lali.ly"

% look for lalily paper/layout/midi
\includeOncePattern "." "^paper(\..*)?\.ly$" % once?
\includeOncePattern "." "^layout(\..*)?\.ly$" % once?
\includeOncePattern "." "^midi(\..*)?\.ly$" % once?
% look for extensions
\includeOncePattern "extensions" "^.*\.ly$" % once?
\includeOncePattern "../lalily-extensions" "^.*\.ly$" % once?
\includeOncePattern "../../lalily-extensions" "^.*\.ly$" % once?

% TODO look for local lalily-extensions?
% look for local paper/layout/midi
\includeOnceIfExists "lalily-paper.ly" % once?
\includeOnceIfExists "lalily-layout.ly" % once?
\includeOnceIfExists "lalily-midi.ly" % once?

% look for local config
\applyConfig lalilyConfig #registerConfig
%{
\includeOnceIfExists "lalily-config.ly" %once?
#(if (and (defined? 'lalilyConfig)(list? lalilyConfig))
     (for-each (lambda (p)
                 (if (and (pair? p)(list? (car p)))
                     (begin
                      (set-registry-val (car p) (cdr p))
                      ))
                 ) lalilyConfig))
%}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Std layout

#(define (do-layout parser)
   (and
    (not (eq? #t (ly:parser-lookup 'lalilyNoOutputDef)))
    (not (eq? #t (get-registry-val lalily:layout:no-auto-load)))
    (not (defined? 'lalily-no-output-def))
    (not (ly:get-option 'lalily-no-output-def))
    ))

% if allowed, set global paper and layout with default
\includeRelIf "output-default.ly"
#(lambda (parser location)
   (let ((ret (do-layout parser)))
     (if (and (not ret) (lalily:verbose))
         (ly:message "no lalily output-defs!"))
     ret))

#(let ((gss (get-registry-val '(lalily paper global-staff-size))))
   (if (and (do-layout (*parser*)) (number? gss)) (set-global-staff-size gss)))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% look for lalily templates
\includeOncePattern "." "^templates\.?.*\.ly$" % once?
% look for local templates
\includeOnceIfExists "lalily-templates.ly" % once?

% look for editions
\includeOnceIfExists "lalily-edition.ly" % once?



%{
convert-ly (GNU LilyPond) 2.19.36  convert-ly: Processing `'...
Applying conversion: 2.19.2, 2.19.7, 2.19.11, 2.19.16, 2.19.22,
2.19.24, 2.19.28, 2.19.29, 2.19.32
%}
