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

\version "2.16.0"

% stencil: print lyric extender with applications
#(define (lyric-extender::print-applic grob)
   (let* ((estil (ly:lyric-extender::print grob)) ; the base-extender-stencil
          (head-array (ly:grob-object grob 'heads)) ; note heads
          (head-list (ly:grob-array->list head-array)) ; as list
          (x-ref (ly:grob-common-refpoint-of-array grob head-array X))) ; X-reference for calculating positions
     ; look in each note head for 'lyric-extender-applic
     (for-each (lambda (nh)
                 (let* ((mus (ly:event-property (ly:grob-property nh 'cause) 'music-cause)) ; note-event related to this head
                        (exta (ly:music-property mus 'lyric-extender-applic #f))) ; markup to put on the extender
                   (if (markup? exta) ; if markup is found
                       (let* ((alx (ly:music-property mus 'lyric-extender-align-x CENTER)) ; x-alignment (def: CENTER)
                              (aly (ly:music-property mus 'lyric-extender-align-y DOWN)) ; y-alignment (def: DOWN)
                              (stil (grob-interpret-markup grob exta)) ; stencil for markup
                              
                              (x1-off (ly:grob-relative-coordinate nh x-ref X)) ; notehead position on x-axis
                              (x2-off (ly:grob-relative-coordinate grob x-ref X)) ; extender position on x-axis
                              (nhext (ly:grob-property nh 'X-extent)) ; notehead extent
                              (x-off (+ (- x1-off x2-off) (/ (- (cdr nhext) (car nhext)) 2))) ; needed x-offset
                              (estm (markup #:translate (cons x-off 0) #:general-align X alx #:general-align Y aly exta)) ; translated markup
                              (esta (grob-interpret-markup grob estm))) ; stencil
                         (set! estil (ly:stencil-add estil esta)) ; add stencil to extender
                         ))
                   ))
       head-list)
     estil))

% default layout specs
\registerLayout #'(lalily default) \layout {
  \context {
    \ChoirStaff
    \name "SemiChoirStaff"
    \consists "Span_bar_engraver"
    \override SpanBar #'stencil =
      #(lambda (grob) 
        (if (string=? (ly:grob-property grob 'glyph-name) "|")
            (set! (ly:grob-property grob 'glyph-name) ""))
        (ly:span-bar::print grob))
  }
  \context {
    \StaffGroup
    \name "ChoirGroup"
    \override SpanBar #'transparent = ##t
  }
  \context {
    \Score
    \accepts SemiChoirStaff
    \accepts ChoirGroup
    \override LyricExtender #'stencil = #lyric-extender::print-applic
  }
  
  \context {
    \Score
    \override MetronomeMark #'padding = #3
    \override BarNumber #'stencil = #(make-stencil-rboxer 0.1 0.1 ly:text-interface::print)    
    \override BarNumber #'font-size = #-1
    
    \override StaffGrouper #'default-staff-staff-spacing = #'((basic-distance . 12)
     (minimum-distance . 1)
     (padding . 1)
     (stretchability . 6))
    \override StaffGrouper #'nonstaff-relatedstaff-spacing = #'((basic-distance . 6)
     (minimum-distance . 1)
     (padding . 0.5)
     (stretchability . 0))
    \override StaffGrouper #'nonstaff-unrelatedstaff-spacing = #'((basic-distance . 6)
     (minimum-distance . 1)
     (padding . 2)
     (stretchability . 0))
    
    \override StaffGrouper #'nonstaff-nonstaff-spacing = #'((basic-distance . 6)
     (minimum-distance . 1)
     (padding . 1)
     (stretchability . 0))
    
  }
  \context {
    \Score
    \override InstrumentName #'self-alignment-X = #RIGHT
    \override InstrumentName #'padding = #1
  }
  \context {
    \Lyrics
    \override LyricHyphen #'dash-period = #5
    \override LyricHyphen #'length = #.5
  }
  \context {
    \Voice
    \override Script #'padding = #0.5
    \override FootnoteItem #'annotation-line = ##f
  }
  \context { \Voice \remove "Instrument_switch_engraver" }
  \context { \Staff \consists "Instrument_switch_engraver" }
}

% default midi specs
\registerMidi #'(lalily default) \midi {
  \tempo 4 = 120
}
