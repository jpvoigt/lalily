\version "2.19.32"

#(define lalily-relincl-tmp (ly:get-option 'relative-includes))
#(ly:set-option 'relative-includes #t)
\include "../../lalily.ly"
#(ly:set-option 'relative-includes lalily-relincl-tmp)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% A function to position tuplet numbers next to kneed beams on a single
%% staff and between staves. Will ignore tuplets on ordinary beams and
%% with visible brackets.
%%
%% Usage: \override TupletNumber.Y-offset = #kneed-beam
%%
%% You must use manual beaming for this function to work properly.
%%
%% An additional function, called with a separate override (see below), will
%% horizontally center the tuplet number on the kneed beam.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#(define (kneed-beam tuplet-number)
   (let* ((tuplet-bracket (ly:grob-object tuplet-number 'bracket))
          (first-note (ly:grob-parent tuplet-number X))
          (first-stem (ly:grob-object first-note 'stem))
          (beam (if (ly:grob? first-stem) (ly:grob-object first-stem 'beam) #f)))

     (if (and (ly:grob? beam) ; beam on first note?
              (ly:grob-property beam 'knee) ; is it kneed?
              (interval-empty? (ly:grob-property tuplet-bracket 'Y-extent))) ; visible bracket?
         (let* ((stems (ly:grob-object beam 'stems))
                (closest-stem (nearest tuplet-number stems))
                (direction-first-stem (ly:grob-property first-stem 'direction))
                (direction-closest-stem (ly:grob-property closest-stem 'direction))
                (beaming-near-number (car (ly:grob-property closest-stem 'beaming)))
                (beam-multiplier
                 (if (= direction-closest-stem UP)
                     (length (filter positive? beaming-near-number))
                     (length (filter negative? beaming-near-number))))
                (beam-ends (ly:grob-property beam 'positions))
                (mid-beam-Y (/ (+ (car beam-ends) (cdr beam-ends)) 2)) ; mid-beam Y-coordinate
                (number-height (ly:grob::stencil-height tuplet-number))

                ;; inital value of Y-offset (will cause number to overlap beam slightly)
                (correction
                 (- mid-beam-Y
                   (if (= direction-closest-stem UP)
                       (car number-height)
                       (cdr number-height))))
                (beam-width (ly:grob-property beam 'beam-thickness))
                (beam-gap (* 0.5 (ly:grob-property beam 'gap)))
                (beam-padding 0.2)) ; change to move number closer or farther from beam

           ;; refinement of initial value of Y-offset
           (cond
            ((= direction-first-stem direction-closest-stem DOWN)
             (- correction
               (* 0.5 beam-width)
               beam-padding))

            ((= direction-first-stem direction-closest-stem UP)
             (+ correction
               (* 0.5 beam-width)
               beam-padding))

            ((and (= direction-first-stem DOWN) (= direction-closest-stem UP))
             (+ correction
               (* beam-multiplier (+ beam-gap beam-width))
               (* 0.5 beam-width)
               beam-padding))

            ((and (= direction-first-stem UP) (= direction-closest-stem DOWN))
             (- correction
               (* beam-multiplier (+ beam-gap beam-width))
               (* 0.5 beam-width)
               beam-padding)))))))

%% find the stem closest to the tuplet-number
#(define (nearest tuplet-number stems)
   (let* ((refp (ly:grob-system tuplet-number))
          (X-coord (interval-center (ly:grob-extent tuplet-number refp X)))
          (closest (ly:grob-array-ref stems 0)))
     (let lp ((x 1))
       (if (<= (abs (- X-coord
                      (ly:grob-relative-coordinate
                       (ly:grob-array-ref stems x) refp X)))
               (abs (- X-coord
                      (ly:grob-relative-coordinate closest refp X))))
           (set! closest (ly:grob-array-ref stems x)))
       (if (< x (1- (ly:grob-array-length stems)))
           (lp (1+ x))
           closest))))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% A function which horizontally centers a tuplet number on a kneed beam.  May
%% be used in conjunction with the earlier function.
%%
%% Usage: \override  TupletNumber.X-offset = #center-on-beam
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#(define (center-on-beam tuplet-number)
   (let* ((tuplet-bracket (ly:grob-object tuplet-number 'bracket))
          (first-note (ly:grob-parent tuplet-number X))
          (first-stem (ly:grob-object first-note 'stem))
          (beam (if (ly:grob? first-stem) (ly:grob-object first-stem 'beam) #f)))

     (if (and (ly:grob? beam)
              (ly:grob-property beam 'knee)
              (interval-empty? (ly:grob-property tuplet-bracket 'Y-extent)))
         (let* ((refp (ly:grob-system tuplet-number))
                (number-X (interval-center (ly:grob-extent tuplet-number refp X)))
                (beam-center-X (interval-center (ly:grob-extent beam refp X))))

           (- beam-center-X number-X)))))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% music function
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\parserDefine kneeTupletBeam {
  \once \override TupletNumber.Y-offset = #kneed-beam
  \once \override TupletNumber.X-offset = #center-on-beam
}



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
