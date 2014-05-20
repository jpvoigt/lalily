\version "2.18.0"
\include "lalily.ly"

\setMusicFolder \musicPath LY_lvbSB.I

\putMusic meta {
  \numericTimeSignature
  \time 2/4 \key c \minor
  \tempo "Allegro con brio."
  \repeat volta 2 { s2*124 }
  s2*377
}
