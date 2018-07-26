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
         (staff-staff-spacing (assoc-get 'staff-staff-spacing options
                    (get-registry-val template:piano:spacing
                      '((basic-distance . 6)(minimum-distance . 1)(padding . 1)(stretchability . 4)) )))
         (instrument-name (assoc-get 'instrument-name options #f #f))
         )
     ;(ly:message "spacing: ~A" spacing)
     #{
       \new PianoStaff \with {
         \override StaffGrouper.staff-staff-spacing = #staff-staff-spacing
         $(if (ly:context-mod? mods) mods)
         $(if (markup? instrument-name) #{ \with { instrumentName = $instrument-name } #})
         \consists \editionEngraver $piece
       } <<
         \new Staff = $right-name \with {
           $(if (ly:context-mod? smods) smods)
           $(if (ly:context-mod? rmods) rmods)
           \consists \editionEngraver \musicPath right
           annotation-name = "Piano right-hand"
         } <<
           \keepWithTag #'piano-right \getMusicDeep {} #'meta
           \keepWithTag #'piano-right { \getMusic {} global \getMusic right }
         >>
         \new Dynamics \with {
           $(if (ly:context-mod? dmods) dmods)
           \consists \editionEngraver $piece
           \override DynamicText.padding = #1
           annotation-name = "Piano dynamics"
         } <<
           \getMusic {} dynamics
           \getMusicDeep meta
         >>
         \new Staff = $left-name \with {
           $(if (ly:context-mod? smods) smods)
           $(if (ly:context-mod? lmods) lmods)
           \consists \editionEngraver \musicPath left
           annotation-name = "Piano left-hand"
         } <<
           \keepWithTag #'piano-left \getMusicDeep {} #'meta
           \keepWithTag #'piano-left { \getMusic {} global \clef $lclef \getMusic left }
         >>
         \new Dynamics \with {
           $(if (ly:context-mod? pmods) pmods)
           \consists \editionEngraver $piece
           \override DynamicText.padding = #1
           annotation-name = "Piano pedal"
         } <<
           \getMusic {} pedal
           \getMusicDeep meta
         >>
       >>
     #}))

