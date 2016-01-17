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

#(define lalily-relincl-tmp (ly:get-option 'relative-includes))
#(ly:set-option 'relative-includes #t)
\include "../../lalily.ly"
#(ly:set-option 'relative-includes lalily-relincl-tmp)

\setStyle #'TBD \markup {
  \hilite \with-color #red \fromproperty #'style:text
}

% unspanned "cresc/decr" dynamics
\parserDefine #'acresc
#(make-dynamic-script (markup #:normal-text #:style 'dynamic "cresc."))
\parserDefine #'adecr
#(make-dynamic-script (markup #:normal-text #:style 'dynamic "decr."))

% \once \override DynamicText.extra-offset
\parserDefine #'dyneo
#(define-music-function (xy)(number-pair?)
   #{ \once \override DynamicText.extra-offset = $xy #})
% \once \override DynamicText.X-offset
\parserDefine #'dynxo
#(define-music-function (x)(number?)
   #{ \once \override DynamicText.X-offset = #$x #})

% empty dynamicText
\parserDefine #'stopdyn
#(make-dynamic-script (markup #:hspace 0))

% parenthesized pp
\parserDefine #'parenPP
#(make-dynamic-script (markup #:concat (#:general-align Y 0 #:normal-text #:larger #:larger #:larger "("
                                         #:hspace 0.35 #:general-align Y 0 #:dynamic "pp" #:general-align Y 0 #:normal-text #:larger #:larger #:larger ")")))

% breathing with fermata
\parserDefineMusic #'breatheferm {
  \once \override BreathingSign.text = \markup {
    \translate #'(0 . 1.4)
    \override #'(baseline-skip . 1.4)
    \center-column {
      \musicglyph #"scripts.ufermata"
      \musicglyph #"scripts.rcomma"
    }
  } \breathe
}
% breathing with fermata with X-offset
\parserDefine #'breathefermoff
#(define-music-function (off)(number?)
   #{
     \once \override BreathingSign.X-offset = $off
     \breatheferm
   #})

% Dynamic stencils = ##f
\parserDefineMusic #'dynOff {
  \override Hairpin.stencil = ##f
  \override DynamicLineSpanner.stencil = ##f
  \override DynamicText.stencil = ##f
  \override DynamicTextSpanner.stencil = ##f
}
% revert Dynamic stencils
\parserDefineMusic #'dynOn {
  \revert Hairpin.stencil
  \revert DynamicLineSpanner.stencil
  \revert DynamicText.stencil
  \revert DynamicTextSpanner.stencil
}

% once override DynamicText self-alignemnt and X-offset
\parserDefine #'grobdyn
#(define-music-function (align move)(integer? number?)
   #{
     \once \override DynamicText.self-alignment-X = $align
     \once \override DynamicText.X-offset =
     $(lambda (grob)(- (ly:self-alignment-interface::x-aligned-on-self grob) move))
   #})
% left shifted dynamics - useful, to save vertical space e.g. in SATB scores
\parserDefineMusic #'ldyn { \grobdyn #1 #1.3 }

% like grobdyn, using one number pair instead of two arguments
\parserDefine #'moveDyn
#(define-music-function (mov)(number-pair?)
   #{
     \once \override DynamicText.self-alignment-X = $(car mov)
     \once \override DynamicText.X-offset =
     $(lambda (grob)(- (ly:self-alignment-interface::x-aligned-on-self grob) (cdr mov)))
   #})

% lift fermata
\parserDefine #'liftFerm
#(define-music-function (off)(number?)
   #{
     \once \override Script.Y-offset = $off
   #})

% one space, markup
\parserDefineMarkup #'msp \markup { " " }
% left align Lyrics (once)
\parserDefine #'lyrML
#(define-music-function (offx)(number?)
   #{
     \once \override LyricText.self-alignment-X = #LEFT
     \once \override LyricText.X-offset = $offx
   #})

%
\parserDefine #'lyreop
#(define-music-function (mov mus)(number-pair? ly:music?)
   #{ {
     \override LyricText.extra-offset = $mov
     \override LyricHyphen.extra-offset = $mov
     \override LyricExtender.extra-offset = $mov
     $mus
     \revert LyricText.extra-offset
     \revert LyricHyphen.extra-offset
     \revert LyricExtender.extra-offset
   } #})

\parserDefineMusic #'spanVisible {
  \once \override StaffGroup.SpanBar.transparent = ##f
  \once \override StaffGroup.BarLine.allow-span-bar = ##t
}

\parserDefine #'T
#(define-music-function (frac tsd music)
   ((fraction? '(2 . 3)) ly:duration? ly:music?)
   #{
     \once \set tupletSpannerDuration = $(ly:duration-length tsd)
     \times $frac $music
   #})


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% cue

\parserDefine #'cueName
#(define-music-function (name dir align xoff yoff)(string? integer? integer? number? number?)
   #{
     \once \override TextScript.direction = $dir
     \once \override TextScript.self-alignment-X = $align
     \once \override TextScript.extra-offset = $(cons xoff yoff)
     s1*0-\markup \fontsize #-4 { $name }
   #})
\parserDefine #'cueNameUp
#(define-music-function (name)(string?)
   #{
     \cueName $name #UP #LEFT #0 #0
   #})
\parserDefine #'cueNameDown
#(define-music-function (name)(string?)
   #{
     \cueName $name #DOWN #LEFT #0 #0
   #})

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% dynamics

\parserDefine #'Odynx { \once \override DynamicText.X-offset = #0 }
\parserDefine #'Wdynx { \override DynamicText.X-offset = #0 }
\parserDefine #'Rdynx { \revert DynamicText.X-offset }

#(define-markup-command (extdyn layout props text dyn)(markup? string?)
   "markup extent"
   (let* (
           (atr-stencil (interpret-markup layout props (markup #:normal-text #:italic text)))
           (dyn-stencil (interpret-markup layout props (markup #:dynamic dyn)))
           (atr-x-ext (ly:stencil-extent atr-stencil X))
           (dyn-x-ext (ly:stencil-extent dyn-stencil X))
           (atr-x (- (cdr atr-x-ext)(car atr-x-ext)))
           (dyn-x (- (cdr dyn-x-ext)(car dyn-x-ext)))
           (x-align (/ (+ atr-x (/ dyn-x 2)) (+ atr-x dyn-x) ))
           )
     (interpret-markup layout props (markup #:halign x-align #:line (#:normal-text #:italic text #:dynamic dyn)))
     ))
#(define (make-dynamic-script-ext text dyn)
   (make-music 'AbsoluteDynamicEvent
     'tweaks '((X-offset . 0)) 'text (markup #:whiteout #:extdyn text dyn )))
#(define-markup-command (extdynr layout props dyn text)(string? markup?)
   "markup extent"
   (let* (
           (atr-stencil (interpret-markup layout props (markup #:normal-text #:italic text)))
           (dyn-stencil (interpret-markup layout props (markup #:dynamic dyn)))
           (atr-x-ext (ly:stencil-extent atr-stencil X))
           (dyn-x-ext (ly:stencil-extent dyn-stencil X))
           (atr-x (- (cdr atr-x-ext)(car atr-x-ext)))
           (dyn-x (- (cdr dyn-x-ext)(car dyn-x-ext)))
           (x-align (/ (- (+ (/ dyn-x 2) atr-x)) (+ atr-x dyn-x) ))
           )
     (interpret-markup layout props (markup #:halign x-align #:line ( #:dynamic dyn #:normal-text #:italic text )))
     ))
#(define (make-dynamic-script-ext-r dyn text)
   (make-music 'AbsoluteDynamicEvent
     'tweaks '((X-offset . 0)) 'text (markup #:whiteout #:extdynr dyn text )))

#(define (make-dynamic-script-align str aln)
   (make-music 'AbsoluteDynamicEvent
     'tweaks `((self-alignment-X . ,aln))
     'text str))

\parserDefine sempref #(make-dynamic-script-ext "sempre" "f")
\parserDefine semprep #(make-dynamic-script-ext "sempre" "p")
\parserDefine pocop #(make-dynamic-script-ext "poco" "p")
\parserDefine pocof #(make-dynamic-script-ext "poco" "f")
\parserDefine cresd #(make-dynamic-script-align (markup #:whiteout #:normal-text #:italic "cresc.") LEFT)
\parserDefine decrd #(make-dynamic-script-align (markup #:whiteout #:normal-text #:italic "decr.") LEFT)
\parserDefine dimd #(make-dynamic-script-align (markup #:whiteout #:normal-text #:italic "dim.") LEFT)
\parserDefine pococ #(make-dynamic-script-align (markup #:whiteout #:normal-text #:italic "poco cresc.") LEFT)
\parserDefine pocod #(make-dynamic-script-align (markup #:whiteout #:normal-text #:italic "poco decr.") LEFT)

% mehrere (historische) Notenköpfe in eins
\parserDefine hmul
#(let ((pred (lambda (v) (and (list? v)(every number? v)))))
   (define-music-function (pos)(pred)
     (let ((hl (list #{ \markup { \musicglyph #"noteheads.s2" } #}))
           (oy 0)(dx 0))
       (for-each (lambda (y)
                   (if (< y oy) (set! dx (- dx .37)))
                   (set! oy y)
                   (set! hl `(,@hl ,(begin #{ \markup { \translate #`(,dx . ,(/ y 2)) \musicglyph #"noteheads.s2" } #})))
                   ) pos)
       #{
         \once \override NoteHead.stencil = #ly:text-interface::print
         \once \override NoteHead.text = \markup { \concat $hl }
       #})))
\parserDefine hdou {
  \once \override NoteHead.stencil = #ly:text-interface::print
  \once \override NoteHead.text = \markup \concat
  { \musicglyph #"noteheads.s2" \musicglyph #"noteheads.s2" }
}
\parserDefine htri {
  \once \override NoteHead.stencil = #ly:text-interface::print
  \once \override NoteHead.text = \markup \concat
  { \musicglyph #"noteheads.s2" \musicglyph #"noteheads.s2" \musicglyph #"noteheads.s2" }
}
\parserDefine remstem
#(define-music-function (mus)(ly:music?)
   #{
     \override Stem.stencil = ##f
     $mus
     \revert Stem.stencil
   #})



%{
/usr/bin/python: /home/jpv/lily2.17/lilypond/usr/lib/libz.so.1: no
version information available (required by /usr/bin/python) convert-ly
(GNU LilyPond) 2.17.96  convert-ly: »« wird verarbeitet... Anwenden
der Umwandlung: 2.17.0, 2.17.4, 2.17.5, 2.17.6, 2.17.11, 2.17.14,
2.17.15, 2.17.18, 2.17.19, 2.17.20, 2.17.25, 2.17.27, 2.17.29
%}


%{
convert-ly (GNU LilyPond) 2.19.36  convert-ly: Processing `'...
Applying conversion: 2.17.97, 2.18.0, 2.19.2, 2.19.7, 2.19.11,
2.19.16, 2.19.22, 2.19.24, 2.19.28, 2.19.29, 2.19.32
%}
