\version "2.16.0"

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
