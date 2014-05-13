\version "2.18.0"
\include "lalily.ly"

\optionsInit opts
\optionsAdd opts element.choir.template \Path lalily.vocal.group
\optionsAdd opts element.choir.staff-mods \with { midiInstrument = "choir aahs" }
%\optionsAdd opts element.accomp.template \Path lalily.piano

\setDefaultTemplate mutopia.DowlandJ.ComeAgain lalily.group #opts
\setTitle "Come Again"
\setComposer #'("John Dowland" . "(1563-1626)")
\setSubSubTitle "From \"The First book of Songes or Ayres\""
\setSubTitle "Ayres and Lute Songs XVII"
\setHeader #'composition "1597"


\putMusic meta {
  \key c \major
  \time 4/2
  \skip 1*14
  \repeat "volta" 2 {
    \time 2/2
    \skip 1*4
    \time 6/2
    \skip 2*6
    \time 4/2
    \skip 2*4
    \time 6/2
    \skip 2*6
  }
}

\putMusic choir.sop.music \relative c''{
  r2 b4. c8 d1
  r2 d e d |
  c2. c4 b1 |
  r2 d2 d c |
  b2 b a1 |
  r2 a b g |
  a2. a4 a1 |
  r4 d,^\segno g2
  r4 e4 a2 |
  r4 fis4 b2
  r4 g4 c2 |
  r4 a4 d\breve ~ d2
  r4 d c b a2 r4 b4
  a g g2.\melisma fis8[ e]\melismaEnd fis2 g1
}

\putMusic choir.sop.lyrics \lyricmode{
  Come a -- gain: sweet love doth now in -- vite,
  thy gra -- ces that re -- fraine,
  to do me due de -- light,
  to see,
  to heare,
  to touch,
  to kisse,
  to die, __
  with thee a -- gaine
  in sweet -- est sym -- pa --  thy.
}


\putMusic choir.alt.music \relative c''{
  g2. g4 g1
  r2 b2 a4 g g2 ~ g2
  fis2 g1
  r2
  g2 g e4.\melisma fis8\melismaEnd
  g2. g4 fis1
  r2 fis2	g2. d4
  e2. e4 fis1
  d1 e2 %to see
  r4 e4 fis2  % to hear
  r4 fis4 g2 % to touch
  r4 g4   a2 % to kisse
  r4 a4 b1. a2 g4.
  f8 e4 g
  fis2. g4
  e2 b4\melisma c\melismaEnd d2. c4 b1
}

\putMusic choir.alt.lyrics \lyricmode{
  Come a -- gaine, sweet love doth now in -- vite
  thy gra -- ces that re -- fraine,
  to doe me due de -- light
  to see, to heare, to touch, to kisse,
  to die to die with thee a -- gaine in swee -- test
  sym -- pa -- thy
}

\putMusic choir.ten.music \relative c'{
  d2. d4 b1
  r2 b c d e2. d8[ c] d1
  r2 b b a g d' d1
  r2
  d d2. d4 d2
  cis2 d1
  g,1 g2 r4 c4
  a2 r4 d4 b2 r4 e4 d2 r4 c b g g a
  b2 c d r4 g, d'2. d4 c b b a8[ g] a2. a4 g1
}

\putMusic choir.ten.lyrics \lyricmode{
  Come a -- gaine, sweet love doth now in -- vite, thy
  gra -- ces that re -- fraine to do me due de -- light to
  see to heare to touch to kisse to  die to die
  with thee a -- gaine, with thee a -- gaine in swee -- test
  sym -- pa -- thy
}

\putMusic choir.bas.music \relative c'{
  g2 g g1 r2 g, c b a2. a4 g1
  r2 g2 g a b g d'1 r2
  d2 g, b
  a2. a4 d1
  b1 c2. c4 d2. d4 e2. d4 fis2. fis4 g2 g, g a b
  c d b c4\melisma d\melismaEnd e2 d2. d4 g,1
}
\putMusic choir.bas.lyrics \lyricmode{
  Come a -- gaine,
  sweet love doth now in -- vite thy gra -- ces that re --
  fraine to doe mee due de -- light
  to see to heare to touch to kisse
  to die to die with thee a -- gaine in swee -- test sym -- pa --
  thy
}


\putMusic accomp.right \relative c' {
  <b d g>1 ~  <b d g> |
  r2 \context Staff <<
    { g'4 fis e fis  <g d>2 ~| g8[ g fis e] fis2 g1 } \\
    { <d b>2 g, g| c1 <d b>}
  >>
  r2 \context Staff <<
    { g4. a8 b2 a | g2. g4 fis1} \\
    { <b, d>1 <c e>4. fis8 | d2 b a1|}
  >>
  r2 \context Staff <<
    { fis'2 g1 | r8 e4. ~ e4 g fis1} \\
    { <d a>2  <<d1~ \new Voice \relative c' { \voiceFour b2 g}>>  d8 d cis b cis2 <d a>1}
  >>
  <c g>2. <c g>4 <c e>2 r4 <c e>4 |
  <a fis'>2 r4 <d fis>4 <b g'>2 r4 <e g>4 | <d a'>2 r4 <d a'>4
  <d b'>1. <c e a>2 |
  \context Staff <<
    { g'4. fis8 e4 g } \\ { d2 c4 b}
  >>
  <fis' d>2 <g d> |
  <e c>4 b2 c4 \context Staff <<{ d2. c4 } \\ a1>> <b d g>1
}

\putMusic accomp.left \relative c'{
  g1( g,) |
  r2 g'2 c, b |
  a1 g |
  r2 g2. g4 a2 |
  b g d' d, |
  r2 d' g, b |
  <a a'>1 d2 d, |
  b'2. b4 c2 r4 c4 d2 r4 d4 e2 r4 e4 |
  fis2 r4 fis4 g2 g, g'4 g, a2 |
  b2 c <d a'> <b b'>
  <c a'>4 \context Staff <<
    { g'1 g4 fis2 } \\
    { d4 e2 d1}
  >>
  <g, g'>1
}

\lalilyTest
