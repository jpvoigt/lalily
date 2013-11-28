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

% parser-define! ilyStartup to load lalily.ily
% instant scheme expression ($) to allow nested includes
$(ly:parser-define! parser 'ilyStartup
   (if (defined? 'lalily-startup)
       ; lalily.ily already loaded
       (define-music-function (parser location)()
         ;(if (lalily:verbose) (ly:message "lalily already included!"))
         (make-music 'SequentialMusic 'void #t ))
       ; include lalily.ily
       (define-music-function (parser location)()
         (let* ((locname (car (ly:input-file-line-char-column location)))
                (loclen (string-length locname))
                (iname (string-append (substring locname 0 (- loclen 2)) "i" (substring locname (- loclen 2)))))
           (ly:parser-include-string parser (format "\\include \"~A\"\n" iname))
           (make-music 'SequentialMusic 'void #t )))
       ))
\ilyStartup

% write log-file, only if this file is compiled directly
\executeLocal
#(lambda ()
   (write-lalily-log-file parser
     '(persons . #t)
     '(music . #f)
     '(defaults . #f)
     '(quotes . #f)
     '(templates . #t)
     '(template-ref . #f)
     '(edition-mods . #f)
     )
   )



%{
/usr/bin/python: /home/jpv/lily2.17/lilypond/usr/lib/libz.so.1: no
version information available (required by /usr/bin/python) convert-ly
(GNU LilyPond) 2.17.96  convert-ly: »« wird verarbeitet... Anwenden
der Umwandlung: 2.17.0, 2.17.4, 2.17.5, 2.17.6, 2.17.11, 2.17.14,
2.17.15, 2.17.18, 2.17.19, 2.17.20, 2.17.25, 2.17.27, 2.17.29
%}
