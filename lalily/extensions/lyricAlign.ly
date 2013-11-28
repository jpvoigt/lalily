\version "2.17.29"

%%%% This file is part of lalily, an extension to lilypond <http://www.lilypond.org/>.
%%%%
%%%% Copyright (C) 2011--2013 Jan-Peter Voigt <jp.voigt@gmx.de>
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

#(define lalily-relincl-tmp (ly:get-option 'relative-includes))
#(ly:set-option 'relative-includes #t)
\include "../../lalily.ly"
#(ly:set-option 'relative-includes lalily-relincl-tmp)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% get the staff context id and store it in (var)
%%% this function returns an empty dummy engraver but store the context id
%%% of the staff context in var
\parserDefine getStaffID
#(define-scheme-function (parser location var)(string-or-symbol?)
   "stores the context id of a found Staff context in a variable called {var}."
   ; make a symbol, if string is given
   (if (string? var)(set! var (string->symbol var)))
   ; return scheme engraver
   (lambda (context)
     (let ((stc (ly:context-find context 'Staff)))
       ; parental staff context is in 'stc'
       (if (ly:context? stc)
           ; store staff context id in variable with name given in 'var'
           (ly:parser-define! parser var (ly:context-id stc))
           ; if no staff context is found display warning
           (ly:input-warning location "no Staff found!")
           ))
     ; return an empty list as dummy engraver
     '())
   )

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% the returned engraver sets the alignAboveContext or alignBelowContext property
%%% in the initialize funtion
\parserDefine alignContext
#(define-scheme-function (parser location dir var)(integer? string-or-symbol?)
   "set alignAboveContext or alignBelowContext property resp."
   ; make a symbol, if string is given
   (if (string? var)(set! var (string->symbol var)))
   ; return scheme engraver
   (lambda (context)
     (let ((stid (ly:parser-lookup parser var)))
       ; context id is in 'stid'
       (if (and (string? stid)(not (string-null? stid)))
           ; initialize with alignAbove/BelowContext
           `((initialize . ,(lambda (trans)
                              (ly:context-set-property! context
                                (if (< dir 0) 'alignBelowContext 'alignAboveContext) stid))))
           ; give a warning if no context id is found
           (begin
            (ly:input-warning location "no context ID!")
            '())
           ))))





%{
/usr/bin/python: /home/jpv/lily2.17/lilypond/usr/lib/libz.so.1: no
version information available (required by /usr/bin/python) convert-ly
(GNU LilyPond) 2.17.96  convert-ly: »« wird verarbeitet... Anwenden
der Umwandlung: 2.17.0, 2.17.4, 2.17.5, 2.17.6, 2.17.11, 2.17.14,
2.17.15, 2.17.18, 2.17.19, 2.17.20, 2.17.25, 2.17.27, 2.17.29
%}
