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

% parser-define! ilyStartup to load lalily/bootstrap.ily
% instant scheme expression ($) to allow nested includes
$(ly:parser-define! 'ilyStartup
   (if (defined? 'lalily-startup)
       ; lalily.ily already loaded
       (define-music-function ()()
         ;(if (lalily:verbose) (ly:message "lalily already included!"))
         (make-music 'SequentialMusic 'void #t ))
       ; include lalily/bootstrap.ily
       (define-music-function ()()
         (let* ((locname (car (ly:input-file-line-char-column (*location*))))
                (loclen (string-length locname))
                (iname (string-append (substring locname 0 (- loclen 3)) "/bootstrap.ily")))
           (ly:parser-include-string (format "\\include \"~A\"\n" iname))
           (make-music 'SequentialMusic 'void #t )))
       ))
\ilyStartup

% write log-file, only if this file is compiled directly
\executeLocal
#(lambda ()
   (write-lalily-log-file
     '(persons . #t)
     '(music . #f)
     '(defaults . #f)
     '(quotes . #f)
     '(templates . #t)
     '(template-ref . #f)
     '(edition-mods . #f)
     )
   )

