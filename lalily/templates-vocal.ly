\version "2.17.29"

#(define lalily-relincl-tmp (ly:get-option 'relative-includes))
#(ly:set-option 'relative-includes #t)
\include "../lalily.ly"
#(ly:set-option 'relative-includes lalily-relincl-tmp)

\registerTemplate #'(lalily vocal)
#(define-music-function (parser location piece options)(list? list?)
   (let ((clef (assoc-get 'clef options "G" #f))
         (vocname (assoc-get 'vocname options #f #t))
         (staff-mods (assoc-get 'staff-mods options #f #f))
         (voice-mods (assoc-get 'voice-mods options #f #f))
         (lyric-mods (assoc-get 'lyric-mods options #f #f)))
     #{
       <<
         \new Staff \with {
           $(if (ly:context-mod? staff-mods) staff-mods #{ \with {} #})
           \consists \editionEngraver $piece
         } \new Voice = $vocname \with {
           $(if (ly:context-mod? voice-mods) voice-mods #{ \with {} #})
         } <<
           \getMusicDeep #'meta
           { \callTemplate #'(/ global voice) #'() #'() \clef $clef \getMusic #'(music) }
         >>
         \new Lyrics \with {
           $(if (ly:context-mod? lyric-mods) lyric-mods #{ \with {} #})
           \consists \editionEngraver $piece
         }\lyricsto $vocname \getMusic #'(text)
       >>
     #}))


%{
/usr/bin/python: /home/jpv/lily2.17/lilypond/usr/lib/libz.so.1: no
version information available (required by /usr/bin/python) convert-ly
(GNU LilyPond) 2.17.96  convert-ly: »« wird verarbeitet... Anwenden
der Umwandlung: 2.17.0, 2.17.4, 2.17.5, 2.17.6, 2.17.11, 2.17.14,
2.17.15, 2.17.18, 2.17.19, 2.17.20, 2.17.25, 2.17.27, 2.17.29
%}
