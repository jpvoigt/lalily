\version "2.19.49"

#(define lalily-relincl-tmp (ly:get-option 'relative-includes))
#(ly:set-option 'relative-includes #t)
\include "../lalily.ly"
#(ly:set-option 'relative-includes lalily-relincl-tmp)

\registerTemplate lalily.piano
#(define-music-function (piece options)(list? list?)
   (let ((mods (assoc-get 'context-mods options #f #f))
         (smods (assoc-get 'staff-mods options #f #f))
         (rmods (assoc-get 'right-mods options #f #f))
         (lmods (assoc-get 'left-mods options #f #f))
         (dmods (assoc-get 'dynamic-mods options #f #f))
         (pmods (assoc-get 'pedal-mods options #f #f))
         (rclef (assoc-get 'right-clef options "G" #f))
         (lclef (assoc-get 'left-clef options "bass" #f))
         (right-name (assoc-get 'right-name options "right" #f))
         (left-name (assoc-get 'left-name options "left" #f))
         )
     (ly:message "left-name: ~A" left-name)
     #{
       \new PianoStaff \with {
         $(if (ly:context-mod? mods) mods)
         \consists \editionEngraver $piece
         \override StaffGrouper.staff-staff-spacing =
         #'((basic-distance . 6)(minimum-distance . 1)(padding . 1)(stretchability . 4))
       } <<
         \new Staff = $right-name \with {
           $(if (ly:context-mod? smods) smods)
           $(if (ly:context-mod? rmods) rmods)
           \consists \editionEngraver \musicPath right
         } <<
           \keepWithTag #'piano-right \getMusicDeep {} #'meta
           \keepWithTag #'piano-right { \getMusic {} global \getMusic right }
         >>
         \new Dynamics \with {
           $(if (ly:context-mod? dmods) dmods)
           \consists \editionEngraver $piece
           \override DynamicText.padding = #1
         } { \getMusic {} dynamics }
         \new Staff = $left-name \with {
           $(if (ly:context-mod? smods) smods)
           $(if (ly:context-mod? lmods) lmods)
           \consists \editionEngraver \musicPath left
         } <<
           \keepWithTag #'piano-left \getMusicDeep {} #'meta
           \keepWithTag #'piano-left { \getMusic {} global \clef $lclef \getMusic left }
         >>
         \new Dynamics \with {
           $(if (ly:context-mod? pmods) pmods)
           \consists \editionEngraver $piece
           \override DynamicText.padding = #1
         } \getMusic {} pedal
       >>
     #}))

