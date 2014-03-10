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

#(define lalily-relincl-tmp (ly:get-option 'relative-includes))
#(ly:set-option 'relative-includes #t)
\include "../lalily.ly"
#(ly:set-option 'relative-includes lalily-relincl-tmp)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% utilities

\parserDefine getLyrics
#(define-music-function (parser location path)(list?)
   #{
     % LineBreakEvent in Lyrics disturbs line-breaks in music
     \filterMusic #'(LineBreakEvent) \getMusic $path
   #})

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Generic

\registerTemplate #'(generic)
#(define-music-function (parser location piece options)(list? list?)
   (get-music piece location))

\registerTemplate #'(NOTFOUND)
#(define-music-function (parser location piece options)(list? list?)
   (ly:input-message location "No template specified for [~A]!" (glue-list piece "."))
   (get-music piece location))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Transpose

\registerTemplate #'(transpose)
#(define-music-function (parser location piece options)(list? list?)
   (let ((template (ly:assoc-get 'template options #f #f))
         (opts (let ((pce (ly:assoc-get 'piece options #f #f))) (if pce (get-default-options pce location) options)))
         (pce (ly:assoc-get 'piece options piece #f))
         (pdiff (ly:assoc-get 'transpose options piece #f) )
         )
     (if (not (list? pce))(set! pce (list pce)))
     (ly:music-transpose (ly:music-deep-copy
                          (call-template template parser location pce options)
                          ) pdiff)
     ))
#(define-public setTransposedTemplate (define-music-function (parser location t1 t2 piece tmpl options)(ly:pitch? ly:pitch? list? list? list?)
                                        (set-default-template piece '(transpose) (assoc-set-all! options `((transpose . ,(ly:pitch-diff t2 t1)) (template . ,tmpl))))
                                        (make-music 'SequentialMusic 'void #t)))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% global voice

\registerTemplate #'(Voice)
#(define-music-function (parser location piece options)(list? list?)
   (let ((init (assoc-get 'init options #f)))
     #{
       \new Voice { $(if (ly:music? init) init) \getMusic #'() }
     #}))

\registerTemplate #'(Lyrics)
#(define-music-function (parser location piece options)(list? list?)
   (let ((init (assoc-get 'init options #f)))
     #{
       \new Lyrics { $(if (ly:music? init) init) \getMusic #'() }
     #}))

\registerTemplate #'(global voice)
#(define-music-function (parser location piece options)(list? list?)
   (let* ((localsym (assoc-get 'localsym options '(global) #f))
          (deepsym (assoc-get 'deepsym options 'global-voice #f))
          (deepm #{ \getMusicDeep { \dynamicUp \autoBeamOff } #deepsym #}))
     #{
       \getMusic $deepm $localsym
     #}))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% lead-sheet template

\registerTemplate #'(lead-sheet lyrics)
#(define-music-function (parser location piece options)(list? list?)
   (let ((lyric (ly:assoc-get 'lyric options (cons 'A #f) #f))
         (rr (ly:assoc-get 'repeats options #f #f))
         (stanza (ly:assoc-get 'stanza options #t #f))
         (voice (ly:assoc-get 'voice options "melody" #f)))
     (if rr
         (let ((c 0))
           (make-music 'SimultaneousMusic 'elements
             (map (lambda (r)
                    (set! c (+ 1 c))
                    #{
                      \keepWithTag $r \new Lyrics \with {
                        \consists \editionEngraver \musicPath $(list (car lyric))
                      } \lyricsto { $voice }
                      {
                        $(if (and stanza (= c 1)) #{ \set stanza = $(cdr lyric) #}) \getLyrics $(list (car lyric))
                      }
                    #}) rr)))
         #{
           \new Lyrics \with {
             \consists \editionEngraver \musicPath $(list (car lyric))
           } \lyricsto { $voice }
           {
             $(if stanza #{ \set stanza = $(cdr lyric) #}) \getLyrics $(list (car lyric))
           }
         #})
     ))

\registerTemplate #'(lead-sheet)
#(define-music-function (parser location piece options)(list? list?)
   #{
     <<
       \new ChordNames \getMusic { } #'(chords)
       \new Staff \with {
         \consists \editionEngraver $piece
       } \new Voice = "melody" <<
         \getMusicDeep #'meta
         { \callTemplate #'(/ global voice) #'() #'() \getMusic #'(melody) }
       >>
       \stackTemplate ##f #'(lyrics) ##f #'(lyrics) $(assoc-set! options 'voice "melody")
       #'lyric $(let ((st 0))
                  (map
                   (lambda (v) (begin (set! st (+ 1 st))(cons v (format "~A." st))))
                   (ly:assoc-get 'verses options (get-music-keys (create-music-path #f '(lyrics)) location))))
     >>
   #})

%%% lied-zeile

\registerTemplate #'(lied zeile)
#(define-music-function (parser location piece options)(list? list?)
   #{
     <<
       \new Staff \with {
         \consists \editionEngraver $piece
       } \new Voice = "melodie" <<
         \getMusicDeep #'meta
         { \autoBeamOff \dynamicUp \getMusic #'(noten) }
       >>
       \new Lyrics \with {
         \consists \editionEngraver $piece
       } \lyricsto "melodie" { \getLyrics #'(text) }
     >>
   #})

%%% EKG Lied

\registerTemplate #'(EKG)
#(define-music-function (parser location piece options)(list? list?)
   (let ((verses (ly:assoc-get 'verses options (get-music-keys (create-music-path #f '(text)) location) '(1)))
         (repeats (ly:assoc-get 'repeats options #f #f)))
     #{
       <<
         \new Staff \with {
           \consists \editionEngraver $piece
         } \new Voice = "melodie" <<
           { \getMusic { \numericTimeSignature } #'(global) \getMusicDeep #'meta }
           { \callTemplate #'(/ global voice) #'() #'() \getMusic #'(melodie) }
         >>
         $(make-music 'SimultaneousMusic
            'elements (if (list? repeats)
                          (map (lambda (v)
                                 (make-music 'SimultaneousMusic
                                   'elements (map (lambda (r)
                                                    #{
                                                      \new Lyrics \with {
                                                        \consists \editionEngraver \musicPath #(list v r)
                                                      } \lyricsto "melodie" { \keepWithTag $r \getMusic $`(text ,v) }
                                                    #}) repeats)))
                            verses)
                          (map (lambda (v)
                                 #{
                                   \new Lyrics \with {
                                     \consists \editionEngraver \musicPath #(list v)
                                   } \lyricsto "melodie" { \getLyrics $`(text ,v) }
                                 #}) verses)
                          ))
       >>
     #}))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Choral group

\registerTemplate #'(choral group staff lyrics)
#(define-music-function (parser location piece options)(list? list?)
   (let ((v (ly:assoc-get 'verse options '() #f))
         (rr (ly:assoc-get 'repeats options #f #f))
         (voc (ly:assoc-get 'lyric-voice options "sop" #f)))
     (if rr
         (make-music 'SimultaneousMusic 'elements
           (map (lambda (r)
                  #{
                    \keepWithTag $r \new Lyrics \with {
                      \consists \editionEngraver \musicPath $`(text ,@v)
                    } \lyricsto $voc { \getLyrics $`(text ,@v) }
                  #}) rr))
         #{
           \new Lyrics \with {
             \consists \editionEngraver \musicPath $`(text ,@v)
           } \lyricsto $voc { \getLyrics $`(text ,@v) }
         #}
         )))

#(let* ((i 0)
        (inc (lambda () (set! i (+ i 1)) i)))
   (register-template '(choral group staff)
     (define-music-function (parser location piece options)(list? list?)
       (if (not (list? piece))(set! piece (list piece)))
       (let* ((staff (ly:assoc-get 'staff options '() #f))
              (prefix (ly:assoc-get 'prefix options "" #f))
              (vocname (if (pair? staff) (car staff) (format "voice-~A" (inc))))
              (vocsym #f)
              (vocpname #f)
              (vocpath `(noten ,(cond ((symbol? vocname) vocname)
                                  ((string? vocname) (string->symbol vocname))
                                  (else (string->symbol (format "~A" vocname)))
                                  )))
              (staffopts (if (pair? staff) (cdr staff) '()))
              (clef (ly:assoc-get 'clef staffopts "G" #f))
              (inst (ly:assoc-get 'inst staffopts #f #f))
              (sinst (ly:assoc-get 'sinst staffopts #f #f))
              (mods (ly:assoc-get 'mods staffopts #f #f)))
         (if (not (string? vocname))(set! vocname (format "~A" vocname)))
         (set! vocpname (string-append prefix vocname))
         (set! vocsym (string->symbol vocname))
         (set! options (assoc-set! options 'lyric-voice vocpname))
         #{
           <<
             \new Staff = $vocpname \with {
               $(if (ly:context-mod? mods) mods #{ \with {} #})
               \consists \editionEngraver \musicPath $vocpath
               instrumentName = $inst
               shortInstrumentName = $sinst
             } \new Voice = $vocpname <<
               { \mergeRestsOn \clef $clef \getMusicDeep #'meta }
               { \callTemplate #'(/ global voice) #'() #'() \getMusic $vocpath }
             >>
             \stackTemplate ##f #'(lyrics) ##t $piece $options
             #'verse $(let ((verses (ly:assoc-get 'verses options #f #f )))
                        (if (list? verses)
                            (map (lambda (v) (list (string->symbol vocname) v)) verses)
                            `((,vocsym))))
             %%\new Lyrics \lyricsto $(string-append prefix vocname)
             %%{ \getMusic $`(text ,(string->symbol vocname)) }
           >>
         #}))
     ))
% standard is SATB
\registerTemplate #'(choral group)
#(define-music-function (parser location piece options)(list? list?)
   (if (not (list? piece))(set! piece (list piece)))
   (let ((staffs (ly:assoc-get 'staffs options '((sop . ((inst . "S")))
                                                 (alt . ((inst . "A")))
                                                 (ten . ((clef . "G_8")
                                                         (inst . "T")))
                                                 (bas . ((clef . "bass")
                                                         (inst . "B")))
                                                 ) #f))
         (kat (ly:assoc-get 'keep-alive-together options #t #f))
         (mensur (ly:assoc-get 'mensur options #f #f))
         (mods (ly:assoc-get 'mods options #f #f)))
     #{
       \new StaffGroup \with {
         $(if kat #{ \with { \consists "Keep_alive_together_engraver" } #})
         $(if (ly:context-mod? mods) mods)
         \override BarLine.allow-span-bar = $(if mensur #t #f )
         %\override SpanBar.transparent = $(if (ly:assoc-get 'mensur options #f #f) #f #t )
         \override BarLine.transparent = $(if mensur #t #f )
       } <<
         \stackTemplate ##f #'(staff) ##t $piece $options #'staff $staffs
       >>
     #} ))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Choral lied group

\registerTemplate #'(choral lied staff)
#(define-music-function (parser location piece options)(list? list?)
   (let* ((staff (ly:assoc-get 'staff options '((inst . "S")(voc . sop)) #f))
          (inst (ly:assoc-get 'inst staff "X" #f))
          (voc (ly:assoc-get 'voc staff 'voc #f))
          (vocs (symbol->string voc))
          (clef (ly:assoc-get 'clef staff "G" #f))
          (vv (ly:assoc-get 'verses options (get-music-keys (create-music-path #f '(text)) location) #f)))
     #{
       <<
         \new Staff = $vocs \with {
           \consists \editionEngraver \musicPath $`(noten ,voc)
           instrumentName = $inst
         } \new Voice = $vocs <<
           { \mergeRestsOn \clef $clef \getMusicDeep #'meta }
           { \callTemplate #'(/ global voice) #'() #'() \getMusic $`(noten ,voc) }
         >>
         \stackTemplate ##f #'(lyrics) ##f #'() $(assoc-set-all! options `((lyric-voice . ,vocs))) #'verse $vv
       >>
     #}))
\registerTemplate #'(choral lied staff lyrics)
#(define-music-function (parser location piece options)(list? list?)
   (let ((v (ly:assoc-get 'verse options 'A #f))
         (rr (ly:assoc-get 'repeats options #f #f))
         (voc (ly:assoc-get 'lyric-voice options "sop" #f)))
     (if rr
         (make-music 'SimultaneousMusic 'elements
           (map (lambda (r)
                  #{
                    \keepWithTag $r \new Lyrics \with {
                      \consists \editionEngraver \musicPath $`(text ,v)
                    } \lyricsto $voc { \getMusic $`(text ,v) }
                  #}) rr))
         #{
           \new Lyrics \with {
             \consists \editionEngraver \musicPath $`(text ,v)
           } \lyricsto $voc { \getLyrics $`(text ,v) }
         #}
         )))

%%% SATB 4 Systeme
\registerTemplate #'(choral lied satb4)
#(define-music-function (parser location piece options)(list? list?)
   (let ((staffs (ly:assoc-get 'staffs options '(
                                                 ((voc . sop)(inst . "S"))
                                                 ((voc . alt)(inst . "A"))
                                                 ((voc . ten)(inst . "T")(clef . "G_8"))
                                                 ((voc . bas)(inst . "B")(clef . "bass"))) #f)))
     #{
       \new StaffGroup \with {
         \override SpanBar.transparent = ##t
       } \stackTemplate ##f #'(.. staff) ##f #'() $options #'staff $staffs
     #}))

%%% SATB 2 Systeme
\registerTemplate #'(choral lied satb2)
#(define-music-function (parser location piece options)(list? list?)
   (let ((sa-clef (assoc-get 'sa-clef options "G"))
         (tb-clef (assoc-get 'tb-clef options "bass"))
         (vocs (assoc-get 'lyric-voice options "sop"))
         (vv (assoc-get 'verses options (get-music-keys (create-music-path #f '(text)) location))))
     #{
       \new StaffGroup \with {
         \override SpanBar.transparent = ##t
       } <<
         \new Staff = "SA" \with {
           \consists \editionEngraver \musicPath #'(noten SA)
         } <<
           \new Voice = "sop" { \callTemplate #'(/ global voice) #'() #'() \voiceOne \getMusic #'(noten sop) }
           \new Voice = "alt" { \callTemplate #'(/ global voice) #'() #'() \voiceTwo \getMusic #'(noten alt) }
           { \mergeRestsOn \clef #sa-clef \getMusicDeep #'meta }
         >>
         \stackTemplate ##f #'(.. staff lyrics) ##f #'() $(assoc-set! options 'lyric-voice vocs) #'verse $vv
         \new Staff = "TB" \with {
           \consists \editionEngraver \musicPath #'(noten TB)
         } <<
           \new Voice = "ten" { \callTemplate #'(/ global voice) #'() #'() \voiceOne \getMusic #'(noten ten) }
           \new Voice = "bas" { \callTemplate #'(/ global voice) #'() #'() \voiceTwo \getMusic #'(noten bas) }
           { \mergeRestsOn \clef #tb-clef \getMusicDeep #'meta }
         >>
       >>
     #}))

\registerTemplate #'(choral lied satb2b)
#(define-music-function (parser location piece options)(list? list?)
   (let ((vocs (assoc-get 'lyric-voice-bass options "bas"))
         (vv (ly:assoc-get 'verses options (get-music-keys (create-music-path #f '(text)) location) #f)))
     #{
       <<
         \keepWithTag #'bosso \callTemplate ##f #'(.. satb2) ##t $piece $options
         \keepWithTag #'bassi \stackTemplate ##f #'(.. staff lyrics) ##t $piece $(assoc-set! options 'lyric-voice vocs)  #'verse $vv
       >>
     #}))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Piano

\registerTemplate #'(piano)
#(define-music-function (parser location piece options)(list? list?)
   (let ((mods (assoc-get 'context-mods options #f #f)))
     #{
       \new PianoStaff \with {
         $(if (ly:context-mod? mods) mods)
         \override StaffGrouper.staff-staff-spacing = #'((basic-distance . 6)(minimum-distance . 1)(padding . 1)(stretchability . 4))
       } <<
         \new Staff = "right" \with {
           \consists \editionEngraver \musicPath #'(right)
         } <<
           \keepWithTag #'piano-right \getMusicDeep #'meta
           \keepWithTag #'piano-right { \getMusic {} #'(global) \getMusic #'(right) }
         >>
         \new Dynamics \with {
           \consists \editionEngraver $piece
           \override DynamicText.padding = #1
         } { \getMusic {} #'(dynamics) }
         \new Staff = "left" \with {
           \consists \editionEngraver \musicPath #'(left)
         } <<
           \keepWithTag #'piano-left \getMusicDeep #'meta
           \keepWithTag #'piano-left { \getMusic {} #'(global) \clef $(ly:assoc-get 'piano-left-clef options "bass" #f) \getMusic #'(left) }
         >>
         \new Dynamics \with {
           \consists \editionEngraver $piece
           \override DynamicText.padding = #1
         } \getMusic {} #'(pedal)
       >>
     #}))


%{
/usr/bin/python: /home/jpv/lily2.17/lilypond/usr/lib/libz.so.1: no
version information available (required by /usr/bin/python) convert-ly
(GNU LilyPond) 2.17.96  convert-ly: »« wird verarbeitet... Anwenden
der Umwandlung: 2.17.0, 2.17.4, 2.17.5, 2.17.6, 2.17.11, 2.17.14,
2.17.15, 2.17.18, 2.17.19, 2.17.20, 2.17.25, 2.17.27, 2.17.29
%}
