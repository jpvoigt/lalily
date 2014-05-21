\version "2.18.2"
\include "lalily.ly"

\optionsInit opts
%\optionsAdd opts staff-mods \with { midiInstrument = "choir aahs" }
\setDefaultTemplate jsb.lobet lalily.vocal.group #opts

\setTitle "Lobet den Herrn, alle Heiden (Psalm 117)"
\setSubTitle "BWV 230"
\setComposer #'jsb
\setHeader #'copyright "Public Domain"

\putMusic meta {
  \key c \major
  \time 4/2
  s\breve*98
  \spanVisible \bar "||" \time 3/4
  s2.*67 \spanVisible \bar "|."
}

\putMusic sop.music \relative c {
  r4 c'\melisma e g c e, g c
  e f8[ e]\melismaEnd d[ c] b[ a] b4 g'8[\melisma f] e[ d c b]
  c4\melismaEnd c d d g,\melisma a8[ b] c2~
  c4 bes8[ a] bes2~ bes4\melismaEnd a8[ g] a4 d
  %% 5
  b g c8[\melisma e d f] e2~ e8[ d e f]
  g4\melismaEnd d8[ e] f2~ f4 e8[ d] e2~
  e4 e a, b c2~\melisma c4 d8 e
  d2~ d4 e8 f e2~ e4 d8 c
  d4\melismaEnd c8[ b] c2~ c4 b8[ a] b2
  %% 10
  c4 g'8[\melisma f] e[ d c b] c4\melismaEnd g g\melisma c~\melismaEnd
  c b d8[\melisma c b a] b4\melismaEnd b b cis8 cis
  d4 r4 r2 r1
  r4 e, ( a ) c e\melisma f8[ e] d4\melismaEnd c
  d1~ d4\melisma e8[ f] e4 d\melismaEnd
  %% 15
  c e8[\melisma d] c[ b a gis] a4\melismaEnd e a c8 b
  c4 c8[( b] c4 ) a fis'1~
  fis4 e g f8 e d4 r4 r2
  r4 c2 b4 c c, e\melisma g
  c e, g c e f8[ e]\melismaEnd d[ c] b[ a]
  %% 20
  b4 g'8[\melisma f] e[ d c b] c4\melismaEnd c d d
  g,\melisma a8[ b] c2~ c4 bes8[ a] bes2~
  bes4\melismaEnd a8[ g] a4 d b g c c
  c1~ c4 b4 r4 d4
  e8[\melisma f e d] c[ d c b] a4\melismaEnd g8 a b4 a8[ b]
  %% 25
  c4\melisma d8[ e] f2~ f4\melismaEnd g8[ f] e[ d] c[ b]
  c4 g4 r4 b4 c2~ c4 d8 c
  b4\melisma c8[ d] e[ d]\melismaEnd c[ e] d4 g,4 r4 d'4
  b c8[ d] e4 e8 d c[\melisma d e c] d[ e f d]
  e[ g f g] a[ g f e] d4 e8[ f] g[ f]\melismaEnd e[ d]
  %% 30
  c4 c f f8 g a4 a4 r4 g4~(
  g8 f) e[ f] d4 e8[ f] g4 g c,2~
  c4 b8[ a] b2 c4 g c8[\melisma d c b]
  a4 b8[ c] d[ e]\melismaEnd d[ c] b4 b e8[\melisma f e d]
  c4 d8[ e] f[ g]\melismaEnd f[ e] d4 e8([ f] g[ f]) e[ d]
  %% 35
  cis4\melisma d2 cis4\melismaEnd d2 r4 d
  d8[\melisma e d c] b[ c b a] g4\melismaEnd a8 b c4 c
  c8[\melisma d c bes] a[ bes a g] a4 b8[ c] d2~
  d4\melismaEnd c b e c c r2
  R\breve
  %% 40
  r2 r4 d e8[\melisma f e d] c[ d c b]
  a4\melismaEnd g8 a b4 a8[ b] c4\melisma d8[ e] f2~
  f4\melismaEnd g8[ f] e[ d] e[ c] d4 g,4 r4 d'4
  e8[\melisma f e d] c[ d c b] a4\melismaEnd g8 a b4 a8[ b]
  c4\melisma d8[ e] f2~ f4\melismaEnd g8[ f] e[\melisma d c b]
  %% 45
  c4\melismaEnd f b,2 c4 g c8[\melisma d c bes]
  a[ g a bes] c4\melismaEnd a f a d8[\melisma e d c]
  b[ a b c] d4\melismaEnd a b g d'2~
  d4\melisma c8[ d] e4\melismaEnd a, d d~ d8[ c] b[ a]
  b4 e,4 r4 e'4 e8[\melisma f e d] c[ d c b]
  %% 50
  a4\melismaEnd b8 c d4 d d8[\melisma e d c] b[ c b a]
  g4 a8[ b] c[ b]\melismaEnd c4 b\melisma e~ e8[ dis e fis]
  b,4\melismaEnd b a8[ g] a[ fis] g4 e4 r4 e'8[\melisma d]
  cis4 d2\melismaEnd cis4 d f8[\melisma e] d[ c b a]
  b4\melismaEnd c c( b) c c,\melisma e g
  %% 55
  c e, g c e f8[ e]\melismaEnd d[ c] b[ a]
  b4 g'8[( f] e[ d]) c[ b] c4 a4 r4 d4
  g, a8 b c2~ c4 b8[ a] b2
  c r4 g c2 c
  c ( b4 a ) b2 b
  %% 60
  e1 a,2 d4\melisma e4
  f1. e2~
  e2 d1\melismaEnd c4( b)
  a4. ( gis8 ) a2 gis r4 b
  c b c1\melisma b2
  %% 65
  c1\melismaEnd c2 c
  d2.( c4) b2 bes4\melisma c
  d1 c
  bes a2~ a4\melismaEnd a
  a2 g a r4 a
  %% 70
  d2 d d4 ( cis d2 )
  cis e f4 ( e f2 )
  e r2 r2 d4\melisma e8[ f]
  g4 d g f e d8[ e] f4 e
  d c8[ d] e4 d c a d c
  %% 75
  bes a8[ bes] c4 bes a b\melismaEnd c2
  f4( e) f2 e4 f g f8[ e]
  d2 r2 r1
  R\breve
  R\breve
  %% 80
  r1 r2 c
  c\breve~
  c\breve~
  c2 bes bes r4 a
  g ( c2 ) bes4 a ( g ) a ( bes )
  %% 85
  c ( d ) c ( bes ) a c8 ( bes c4 ) c
  c8[\melisma f, g a] bes[ c]\melismaEnd d bes c4\melisma d8[ e] f[ e]\melismaEnd d[ c]
  b[\melisma a b d] c[ d]\melismaEnd e[ c] d4 e8[ fis] g[ f] e[ d]
  cis[\melisma b cis e] d[ e f d] e4 f8[ g] a[ g]\melismaEnd f[ e]
  f4 a,2 b4 cis ( a a' ) g8 a
  %% 90
  f4 ( e ) f ( d ) cis\melisma d8[ e] f[ e d c]
  b4\melismaEnd a b cis d a d2~
  d cis d r4 a\melisma
  d c8[ b] a4 g8[ a] b4\melismaEnd c d b
  a g8[ fis] g2~ g4\melisma fis8[ g] a4\melismaEnd d,
  %% 95
  d g8([ a] b4) e c d8([ e] f4) d
  g,( a8[ b]) c2 f,4( g8[ a] bes4) g
  a a8[ g] a4 b c\melisma g c2~
  c\melismaEnd b c r2
  c8[ d e d] c[ e]
  %% 100
  d4 g, g'~
  g f8[ e] f[ d]
  e([ f] g4) r4
  r4 g,8[ a] b[ g]
  c4 a d
  %% 105
  b ( e ) c
  g' f8[ e] f[ d]
  e4 g, c~
  c b8[ a] b[ g]
  a[\melisma b c e d c]
  %% 110
  b[ c] d4\melismaEnd g~
  g\melisma f8[ e]\melismaEnd d[ f]
  e4 a,4 r4
  r4 b\melisma e~
  e\melismaEnd d8[ c] b[ d]
  %% 115
  c[\melisma d c b a c]
  b[ a]\melismaEnd b[ d] cis[ e]
  d4 a d~
  d\melisma c8[ b]\melismaEnd a[ c]
  b4 e, e'~
  %% 120
  e c a
  d2 d4~
  d b g
  c8[\melisma b a b c a]
  fis'4\melismaEnd e8([ dis] e4 )
  %% 125
  a,8[ g a fis] b[ a]
  g4 b\melisma e~
  e\melismaEnd d8[ c] b[ d]
  c[\melisma b c e d c]
  b[ a g a b g]
  %% 130
  d'4\melismaEnd r4 r4
  r4 e8[\melisma d c e]
  d4\melismaEnd b e
  a,2.~
  a8[\melisma b c b a c]
  %% 135
  b4\melismaEnd e,\melisma e'~
  e d8[ c]\melismaEnd d[ b]
  c2.
  b
  a8[ b c b a c]
  %% 140
  b[ c d c] b[ d]
  c4 e, a~
  a gis2
  a4 r4 r4
  R2.
  %% 145
  R2.
  e'8[ d c d] e[ c]
  d4 g4 r4
  c,8[ b a b] c[ a]
  b4 e4 r4
  %% 150
  a,8[ g f g] a[ f]
  g4 c8[( b] c4 )
  f2.~
  f4\melisma e8[ d]\melismaEnd e4~
  e8[ f] \grace e8 d4. ( c8 )
  %% 155
  c4 r4 r4
  r4 e8[ f] g4~
  g d g
  r4 c,8[ d] e4~
  e b e
  %% 160
  r4 a,8[ b] c4~
  c g c
  r4 a8[ b c a]
  d4 c8[ b] c4~
  c8 d b4. c8
  %% 165
  c2.\fermata
}


\putMusic sop.lyrics \lyricmode {
  Lo -- bet den Herrn, al -- le, al -- le Hei -- den,  %% T 4
  al -- le Hei -- den, al -- le Hei -- den, al -- le, %% T 7 Anfang
  al -- le Hei -- den, al -- le Hei -- den lo -- bet, %% T 10
  lo -- bet, lo -- bet, lo -- bet den Herrn, %% T 12
  al -- le, al -- le Hei -- den, %% T 15 Anfang
  lo -- bet, lo -- bet den Herrn, al -- le Hei -- den, lo -- bet den Herrn, %% T 17
  al -- le Hei -- den, lo -- bet den Herrn, %% T 20 Anfang
  al -- le, al -- le Hei -- den, al -- le Hei -- den, al -- le Hei -- den, %% T 23
  und prei -- set ihn, al -- le Völ -- ker, al -- le Völ -- ker, %% T 26
  und prei- set ihn, al -- le Völ -- ker, %% T 27
  und prei -- set, prei -- set ihn, al -- le -- Völ -- ker, %% T 29 Anfang
  prei -- set ihn, al -- le, al -- le, al -- le Völ -- ker, al -- le -- Völ -- ker, %% T 32 Mitte
  und prei -- set ihn, und prei -- set ihn, al -- le Völ -- ker, %% T 35
  und prei -- set ihn, al -- le, al -- le, al -- le Völ -- ker, %% T 38
  und prei -- set ihn, al -- le Völ -- ker, al -- le Völ -- ker, %% T 42
  und prei -- set ihn, al -- le -- Völ -- ker, al -- le Völ -- ker, %% T 45
  und prei -- set ihn, und prei -- set ihn, und prei -- set ihn, al -- le -- Völ -- ker, %% T 49
  und prei -- set ihn, al -- le, al -- le Völ -- ker, al -- le -- Völ -- ker, %% T 52
  prei -- set ihn, al -- le Völ -- ker! %% T 54
  Lo -- bet den Herrn, al -- le -- Hei -- den, %% T 56
  und prei -- set ihn, al -- le -- Völ -- ker! %% T 58 Anfang
  Denn sei -- ne Gna -- de und Wahr -- heit wal --  tet_ ü- ber uns, %% T 63
  denn sei -- ne Gna -- de und Wahr -- heit   wal -- tet ü- ber uns, %% T 69
  denn sei -- ne Gna -- de und Wahr -- heit   %% T 72
  wal -- tet ü- ber uns in E -- wig -- keit, %% T 77
  in E-- wig -- keit, %% T83
  in E -- wig -- keit, in  E -- wig -- keit, sei -- ne Gna -- de und Wahr -- heit %% T 86 Ende
  wal -- tet ü- ber uns in E -- wig -- keit, %% T 89 Anfang
  sei -- ne Gna -- de und Wahr -- heit wal -- tet ü -- ber uns in E -- wig -- keit, %% T 92
  wal -- tet ü- ber uns in E -- wig -- keit, %% T 95 Anfang
  sei -- ne Gna -- de __ und Wahr -- heit wal -- tet ü -- ber uns in E -- wig -- keit. %% 98 Ende
  Hal -- le -- lu -- ja, Hal -- le -- lu -- ja, __ %% T 102
  Hal -- le -- lu -- ja, Hal -- le -- lu -- ja, %% T 106 Anfang
  Hal -- le -- lu -- ja, Hal -- le -- lu -- ja, __ %% T 110
  Hal -- le -- lu -- ja, Hal -- le -- lu -- ja, __ Hal -- le -- lu -- ja, %% T 117
  Hal -- le -- lu -- ja, Hal -- le -- lu -- ja, %% T 121
  Hal -- le -- lu -- ja __ Hal -- le -- lu -- ja, %% T 126 Anfang
  Hal -- le -- lu -- ja, __  %% T 130
  Hal -- le -- lu -- ja, __ Hal -- le -- lu -- ja, %% T 138
  Hal -- le -- lu -- ja, Hal -- le -- lu -- ja, %% T 143
  Hal -- le -- lu -- ja, Hal -- le -- lu -- ja, %% T 149
  Hal -- le -- lu -- ja, __ Hal -- le -- lu -- ja, %% T 154
  Hal -- le -- lu -- ja, Hal -- le -- lu -- ja, Hal -- le -- lu -- ja, %% T 161
  Hal -- le -- lu -- ja, Hal -- le -- lu -- ja. %% Ende
}

\putMusic alt.music \relative c {
  R\breve
  r4 g'\melisma b d g b, c g'
  a bes8[ a]\melismaEnd g[ f] e[ d] e4 c'8[\melisma b] a[ g f e]
  f4\melismaEnd f g g c,\melisma d8[ e] f2~
  %% 5
  f4\melismaEnd e8[ f] g4 b, c8[\melisma g' c b] c2~
  c4\melismaEnd b8[ c] d[\melisma e d c] b[ c b a] g4\melismaEnd g
  e2\melisma f g4\melismaEnd e a g
  a ( f ) d g g4 r4 c,4 a'
  g c, c d8 e a,4 f'2 e8[ d]
  %% 10
  e[\melisma f] g4~ g8[ f e d] e4\melismaEnd e e fis8 fis
  g4 d'8[\melisma c] b[ a g fis] g4\melismaEnd d d\melisma g~
  g\melismaEnd f a8[\melisma g f e] f4\melismaEnd f fis gis
  a e c'8[ b a gis] a4 d8([ c] b4) a
  b a gis a b\melisma c8[ d] c4 b\melismaEnd
  %% 15
  a a, ( c ) e a1~
  a~ a4\melisma b8[ c] b4 a
  g1~ g8[ f e d] c[ bes a g]
  f[ g] a4\melismaEnd g g'~ g8[\melisma f e f] g4\melismaEnd f
  e8[\melisma d c d] e4\melismaEnd fis g2~ g4 fis
  %% 20
  g2~ g8[\melisma f e d] c4\melismaEnd c a' g8[ f]
  g4 e e a8 g a4 a2 g4
  g c, c a' g g g fis8 e
  d4 fis ( g ) a g g r2
  R\breve
  %% 25
  r1 r2 r4 g
  a8[\melisma c b a] g[ a g f] e4\melismaEnd d8 e fis4 e8[ fis]
  g4\melisma a8[ b] c2~ c4\melismaEnd d8[ c] b[ a] g[ fis]
  g4 g, r2 r4 g' g g
  g c2 d8[ c] b4 b4 r4 g4
  %% 30
  e f8 g a[\melisma bes a g] f[ g a f] g[ a bes g]
  a[ c bes c] d[ c]\melismaEnd bes[ d] c4 c r2
  a4 f g g g e r4 e4
  c\melisma d8[ e] f[ g]\melismaEnd f[ e] d4 d g8[\melisma a g f]
  e4 f8[ g] a[ bes]\melismaEnd a[ g] f4 g8[\melisma a] bes2~
  %% 35
  bes4\melismaEnd a bes a a8[\melisma bes a g] f[ g f e]
  d4\melismaEnd e8 fis g4 g g8[\melisma a g f] e[ f e d]
  c4 d8[ e] f2~ f4\melismaEnd a gis\melisma a8[ b]
  e,4\melismaEnd e d8[ c] b[ c] a4\melisma e'~ e8[ d e f]
  g4\melismaEnd e r4 d4~ d\melisma c8[ d] e4\melismaEnd c
  %% 40
  f d r2 r4 b' a8[\melisma b a gis]
  a4\melismaEnd f8 f d4 g e8[\melisma g f e] d4\melismaEnd f
  g b c8([ b] c4 ) b d, g8[\melisma a g f]
  e[ d e f] g4\melismaEnd e c2 r2
  r4 g\melisma b d g b, c g'
  %% 45
  a bes8[ a]\melismaEnd g[ f] e[ d] e4 e8([ f] g4) e
  c c r4 c4 f8[\melisma e f g] a4\melismaEnd fis
  b8[\melisma c b a] g[ a g fis] e4\melismaEnd d8 e fis4 e8[ fis]
  g4\melisma a8[ b] c2~ c4\melismaEnd d8[ c] b[ a] gis[ fis]
  gis4 gis a b c a r2
  %% 50
  r4 a a8[\melisma b a g] fis[ g fis e] d4\melismaEnd e8 fis
  g4 g g8[\melisma a g fis] g4 a8[ b] c[ b]\melismaEnd a[ g]
  fis4\melisma e2 dis4\melismaEnd e2 r4 bes'8 bes
  a[\melisma g bes a] g[ f]\melismaEnd g[ e] a4 a, r4 a'4
  d, c8 c a'4 d, g g, r4 d'4
  %% 55
  e b8 c d4 ( e2 ) d4 a' f
  g g r4 g4 g f8 e f2~
  f4\melisma e8[ d] e2~ e4\melismaEnd f d2
  e r4 g g2 fis
  g1 g2 g
  %% 60
  a2. ( g4 ) fis2 f4\melisma g
  a1 g
  f e2~ e4\melismaEnd e
  e2 d e r4 gis
  a2 e f1~
  %% 65
  f2\melisma e4 d\melismaEnd e2 e
  a ( d, ) d g4\melisma a
  bes1. a2~
  a g1\melismaEnd f4 ( e )
  d ( cis ) d2 cis r4 e
  %% 70
  d ( e ) f2 f4 ( e f2 )
  e a a1~
  a4\melisma e f d\melismaEnd e2 r2
  r4 g8[\melisma a] bes4 a g f8[ g] a4 g
  f e8[ f] g4 f e c f e
  %% 75
  d c8[ d] e4 d c d e8[ f]\melismaEnd g4
  f ( a2 ) g4 g2 r4 g
  g\breve~
  g\breve~
  g2 f f r4 e
  %% 80
  d ( g2 ) f4 e ( d ) e ( f )
  g ( a ) g ( f ) g2 g4 g
  g\melisma f8[ e] f[ g a f] g4 f e\melismaEnd f8 g
  a2 d, c4 c d8[ e] f[ d]
  g4 f g2~ g8[\melisma f e g] f4\melismaEnd e8[ d]
  %% 85
  c4 r4 r2 r4 a'8([ g] a4) a
  a( bes8[ a]) g4 f e( f8[ g]) c,4 a'~
  a g8[ f] g4 e fis g8[ a] d,4\melisma g8[ f]
  e4 a~ a8[ g] a4~ a8[ bes a g] f[ e]\melismaEnd d[ cis]
  d4 f8[\melisma e] f[ a g f] e[ d cis e] d[ f e f]
  %% 90
  d4 a'~ a8[ g a f] g[ bes a g] a[ g f e]
  d[ cis d]\melismaEnd f e4 a a,2. a'4
  g ( f ) e2 d4 d2 e4
  fis ( d d' ) c8 d b4 ( a ) b ( g )
  fis\melisma g8[ a] b[ a g fis] e4\melismaEnd d e fis
  %% 95
  g d g1\melisma f2~
  f e1\melismaEnd d2
  c r4 g' c,8[\melisma d e f] g2~
  g4 a\melismaEnd g( f) e2 r2

  R2.
  %% 100
  g8[ a b a] g[ b]
  a4 d, d'~
  d c8[ b] c[ a]
  b([ c] d4) r4
  r4 f,8[ g] a[ f]
  %% 105
  g4 e a
  d, g2
  g4 g8[ b] a[ g]
  fis4 d g
  e d2
  %% 110
  d4 g e
  cis d4 r4
  r4 e\melisma a~
  a g8[ fis]\melismaEnd e[ g]
  fis4 b, b'~
  %% 115
  b a8[\melisma g] a4~
  a\melismaEnd g8[ fis] e4
  a8[ gis a c] b[ a]
  gis4 e4 r4
  r4 e8[ fis] g[ e]
  %% 120
  a4 a, a'~
  a fis d
  g2 g4~
  g e a~
  a g8[ fis] g[ e]
  %% 125
  fis4 fis2
  e8[ fis] g[ fis] e[ g]
  fis4 b, b'~
  b\melisma a8[ g]\melismaEnd fis[ a]
  g4 d g~
  %% 130
  g fis8[ g] a[ fis]
  gis4 a8[ b] a[ g]
  f[ d] e4 r4
  d8[ e f e] d[ f]
  e4 a, a'~
  %% 135
  a\melisma g8[ a]\melismaEnd b[ g]
  a4 d, b'~
  b a8[\melisma gis] a4~
  a\melismaEnd gis8([ fis] gis4)
  a4 r4 r4
  %% 140
  R2.
  c,8[ d e d c e]
  d[ e f e] d[ f]
  e4 a, a'~
  a\melisma g8[ f g e]\melismaEnd
  %% 145
  f2.
  e8[ f] g[ f] e[ g]
  fis4 g4 r4
  e8[ d c d] e[ c]
  f4 e4 r4
  %% 150
  f8[ g a g] f[ a]
  d,4 e8([ f] g4)
  f8[ g] a4 a
  g2 g4
  a ( g ) f
  %% 155
  e\melisma f8[ g8 a b]
  c4\melismaEnd g c~
  c\melisma b8[ a]\melismaEnd b[ g]
  a4 e a~
  a\melisma g8[ f]\melismaEnd g[ e]
  %% 160
  f4 c f~
  f\melisma e8[ d]\melismaEnd e[ c]
  d[\melisma e f g a f]
  g2\melismaEnd g4
  a g2
  %% 165
  g2.\fermata

}

\putMusic alt.lyrics \lyricmode {
  Lo -- bet den Herrn, al --  le, al -- le, Hei -- den, %% T 5 Anfang
  al -- le, al --  le, al -- le,  Hei -- den, %% T 7
  lo -- bet, lo -- bet den Herrn, al -- le Hei --   den, %% T 9
  lo -- bet den Herrn, al -- le Hei -- den, %% T 10
  lo -- bet den Herrn, lo -- bet, lo -- bet, al -- le, al -- le Hei -- den, %% T 13
  al -- le, al -- le Hei -- den, al -- le Hei -- den, %% T 15 Anfang
  al -- le Hei -- den, %% T 18
  al -- le Hei -- den, al -- le Hei -- den, %% T 20
  al -- le Hei -- den, lo -- bet den Herrn, al -- le Hei -- den, %% T 22 Anfang
  al -- le Hei -- den, lo -- bet den Herrn, al -- le Hei -- den, %% Takt 23 Ende
  und prei -- set ihn, al -- le -- Völ -- ker, al -- le -- Völ -- ker, %% T 28
  und prei -- set ihn, al -- le -- Völ -- ker, und prei -- set ihn al -- le Völ -- ker, %% T 31
  al -- le, al -- le Völ -- ker, und prei -- set ihn, und prei -- set ihn, %% T 34
  al -- le Völ -- ker, prei -- set ihn, al -- le, al -- le, %% T 37
  al -- le, al -- le -- Völ -- ker, al -- le Völ -- ker, %% T 40
  und prei -- set ihn, al -- le, al -- le, al -- le Völ -- ker, und prei -- set ihn! %% T 43
  Lo -- bet den Herrn, al -- le Hei -- den, %% T 46
  und prei -- set, prei -- set ihn, al -- le Völ -- ker, al -- le -- Völ -- ker, al -- le Völ -- ker, %% T 49
  und prei -- set ihn, al -- le, al -- le -- Völ -- ker, %% T 52
  al -- le, al -- le -- Völ --  ker, und prei -- set ihn, al -- le Völ -- ker, %% T 54 Ende
  und prei -- set ihn, al -- le, al -- le Völ --   ker, und prei -- set ihn, al -- le Völ -- ker! %% T 58 Anfang
  Denn sei -- ne Gna -- de und Wahr -- heit wal -- tet ü -- ber uns, %% T 63
  denn sei -- ne Gna -- de und Wahr -- heit wal -- tet ü- ber uns, %% T 69
  denn sei -- ne Gna -- de und Wahr -- heit %% T 72
  wal -- tet ü- ber uns in E -- wig -- keit, %% T 79
  in E -- wig -- keit, in E -- wig -- keit, %% T 81
  sei -- ne Gna -- de und Wahr -- heit wal -- tet ü -- ber -- uns in E -- wig -- keit, %% T 85 Anfang
  sei -- ne Gna -- de und Wahr -- heit wal -- tet ü -- ber uns in -- E -- wig -- keit, %% T 89
  wal -- tet ü- ber uns in E -- wig -- keit, %% T 92
  sei -- ne Gna -- de und Wahr -- heit wal -- tet ü- ber uns in E -- wig -- keit, in E -- wig -- keit. %% T 98 Ende
  Hal -- le -- lu -- ja, Hal -- le -- lu -- ja, Hal -- le -- lu -- ja, %% T 103
  Hal -- le -- lu -- ja, Hal -- le -- lu -- ja, Hal -- le -- lu -- ja, Hal -- le -- lu -- ja, %% T 111
  Hal -- le -- lu -- ja, Hal -- le -- lu -- ja, Hal -- le -- lu -- ja, %% T 118
  Hal -- le -- lu -- ja, Hal -- le -- lu -- ja, Hal -- le -- lu -- ja, Hal -- le -- lu -- ja, %% T 126 Anfang
  Hal -- le -- lu -- ja,  Hal -- le -- lu -- ja, Hal -- le -- lu -- ja, Hal -- le -- lu -- ja, %% 132
  Hal -- le -- lu -- ja, Hal -- le -- lu -- ja, Hal -- le -- lu -- ja, %% 139
  Hal -- le -- lu -- ja, Hal -- le -- lu -- ja, %% T 146
  Hal -- le -- lu -- ja, Hal -- le -- lu -- ja, %% T 149
  Hal -- le -- lu -- ja, Hal -- le -- lu -- ja, Hal -- le -- lu -- ja, Hal -- le -- lu -- ja, %% T 158
  Hal -- le -- lu -- ja, Hal -- le -- lu -- ja -- Hal -- le -- lu -- ja.
}

\putMusic ten.music \relative c {
  R\breve
  R\breve
  R\breve
  R\breve
  %% 5
  r4 c\melisma e g c e, g c
  e f8[ e]\melismaEnd d[ c] b[ a] b4 g'8[\melisma f] e[ d c b]
  c4\melismaEnd c d d g,\melisma a8[ b] c2~
  c4 bes8[ a] bes2~ bes4\melismaEnd a8[ g] a4 d
  b g g' f8 e d4 d2 g,4
  %% 10
  g' e r2 r1
  r4 g,( b) d g( a8[ g]) f4 e
  d a'8[\melisma g] f[ e d cis] d4\melismaEnd a a\melisma d~
  d c8[ d] e[ d c b] c4\melismaEnd a f' e
  f1~ f4 gis, a e'
  %% 15
  e c e8[\melisma d c b] c4 a8[ b c d] e4
  dis e\melismaEnd dis e b2 r2
  r4 g\melisma b c g' b, c g'
  a g8[ f]\melismaEnd e4 d e g8([ f] e[ d]) c[ b]
  c4 g c2~ c4\melisma b8[ c] d4\melismaEnd d
  %% 20
  d d r4 g4~ g f8 f f4 b,
  c c~ c8[\melisma b c e] d4\melismaEnd d d e8[ f]
  e4\melisma f8[ g] f[ e d c] d4\melismaEnd d c8[\melisma b a g]
  a4\melismaEnd d g, d' d d r2
  R\breve
  %% 25
  R\breve
  R\breve
  r1 r2 r4 d
  e8[\melisma f e d] c[ d c b] a4\melismaEnd g8 a b4 a8[ b]
  c4\melisma d8[ e] f2~ f4\melismaEnd g8[ f] e[ d] c[ b]
  %% 30
  c4 c r4 c4 f f,8 f c'4 c
  c c r4 g'8[\melisma f] e[ d e g] f[ e d c]
  d4\melismaEnd d d d c c r4 g4
  a8[\melisma b a g] a4\melismaEnd a b8[ c] b[ a] b4 b
  c8[\melisma d c bes] c4\melismaEnd c d8[ e] d[ c] d4 g8([ f]
  %% 35
  e4) f g8[ f] e[ f] d4 d d8[\melisma e d c]
  b[ c b a] g4\melismaEnd a8 b c4 c c8[\melisma d c bes]
  a[ bes a g] f4\melismaEnd a d2~ d8[\melisma c b a]
  gis4 a2\melismaEnd gis4 e' c c2~
  c4\melisma b8[ c] d4\melismaEnd a b g r4 e'4
  %% 40
  d a d8[\melisma c b a] b4\melismaEnd gis e e'
  c d8 d b4 e c2~ c8[\melisma e d c]
  d4\melismaEnd g g g g b, r2
  r4 c,\melisma e g c e, g c
  e f8[ e]\melismaEnd d[ c] b[ a] d4 r4 r4 e8([ d]
  %% 45
  c4 ) d8[ e] d2 g, r4 g
  a8[\melisma bes a g] f[ e f g] a4\melismaEnd f d d'
  d1~ d4~ d8[\melisma c] b[ a g fis]
  e4 e'8[ d] c[ b a gis] a4\melismaEnd gis8 a b4 b
  e, e r2 r4 e' e8[\melisma f e d]
  %% 50
  c[ d c b] a4\melismaEnd b8 c d4 d d8[\melisma e d c]
  b[ c b a] g4\melismaEnd a g\melisma g'2 fis8[ e]
  dis4\melismaEnd e c b b g e'2~
  e4 d e2 d4 d8[\melisma e] f2~
  f4\melismaEnd e d g c, c r2
  %% 55
  r4 g'8([ f] e[ d]) c[ b] a4 a'8([ g] f[ e]) d[ c]
  d4\melisma e8[ f] g4\melismaEnd g e c r4 a'4
  d, g c,8 d([ c b] a4) f g2
  g r4 e' e2 d
  d1 d2 e
  %% 60
  e ( a, ) a r2
  r2 f'4\melisma e d b e d
  c b8[ c] d4 c b gis\melismaEnd a ( b )
  c2 b4 ( a ) b2 r4 e
  e ( d ) c ( b ) a2\melisma g4 f
  %% 65
  g1\melismaEnd g2 a
  a4 ( g a2 ) g r2
  r2 bes4\melisma a g e a g
  f e8[ f] g4 f e cis\melismaEnd d ( e )
  f2 e4 ( d ) e2 r4 cis'
  %% 70
  a2 a a1
  a2 cis d4\melisma cis d2~
  d4 cis\melismaEnd d ( b ) cis\melisma d8[ e] f4 e
  d2 g,4 a8[ bes] c2 f,4 g8[ a]
  bes2~ bes4 a8[ g] a2~ a4 g8[ f]
  %% 75
  g2~ g4 f8[ g] a4 g2\melismaEnd c4
  c2~ c4 b c g c8[ b] c4
  b2 g4 a b ( g g' ) f8 g
  e4 ( d ) e ( c ) b\melisma c8[ d] e[ d c b]
  a4\melismaEnd g a b c g c2~
  %% 80
  c b c4 ( b ) c ( d )
  e ( f ) e ( d ) c e~ e8([ d]) e4
  c2~ c4 c8 d e4 ( d ) c f~ (
  f e ) f g2 f8[ e] d4 c
  bes\melisma a g c2\melismaEnd bes4 a g
  %% 85
  a ( bes ) a ( g ) f f'~ f8([ g]) f[ e]
  d2 d4 g g c, r4 f8([ e]
  d4) d g, g a d8([ c] d4) e
  a, a a d cis d8[ e] a,2~
  a4 d8[\melisma cis] d4 e~ e a8[ g] f4 cis
  %% 90
  d8[ e cis e] d[ e f d] e[ g]\melismaEnd f[ e] d4 a
  f f' g e d4 r4 r2
  r1 r2 d
  d\breve~
  d1. c2
  %% 95
  c4 ( b ) r4 b4 c ( b ) c ( d )
  d2 c4 bes a ( c ) f g~
  g f8[ e] d4 d g2 r4 e
  d ( c ) d2 c r2

  R2.
  %% 100
  R2.
  R2.
  c8[ d e d] c[ e]
  d4 g, g'~
  g f8[ e] f[ d]
  %% 105
  e([ f] g4) r4
  r4 b,8[ c] d[ b]
  c[ b] c[ d] e[ c]
  a4 b8[ c] d4
  c8[ b] a4. g8
  %% 110
  g4 r4 r4
  r4 a\melisma d~
  d c8[ b]\melismaEnd a[ c]
  b4 e8[ d] c[ b]
  a4 b8[ a] g[ fis]
  %% 115
  e[ fis] fis4. g8
  g4 r4 a4~
  a d f
  b, a8[ b c a]
  d[ c b d] c[ b]
  %% 120
  a4 r4 r4
  r4 b fis'
  e2 b4
  r4 e4 c
  b4. b8 b4
  %% 125
  c b2
  b4 r4 r4
  R2.
  R2.
  g8[ a b a] g[ b]
  %% 130
  a4 d, d'~
  d c8[ b] c[ a]
  b[\melisma a b d cis e]
  d4\melismaEnd a\melisma d~
  d\melismaEnd c8([ d] e4)
  %% 135
  e ( b ) g
  c ( a ) f'
  gis, e' c
  f ( d ) b
  c e f
  %% 140
  d ( b ) e
  a,8[ b c b a c]
  b[ c d c] b[ d]
  c[\melisma d e d c e]
  d[ c b d c b]
  %% 145
  a[ c b a b g]\melismaEnd
  g'[ f e f] g[ e]
  a4 d,4 r4
  c8[ d e d] c[ e]
  d[ c] b4 r4
  %% 150
  c8[ bes a bes] c[ d]
  b4 g e'~
  e\melisma d8[ c]\melismaEnd d4~
  d c8[ b] c4~
  c8 d b4. c8
  %% 155
  c[ g a b c d]
  e[ d c d] e[ c]
  d4 g4 r4
  c,8[ b a b] c[ a]
  b4 e4 r4
  %% 160
  a,8[ g f g] a[ f]
  g4 c8([ b] c4)
  f2.~
  f4\melisma e8[ d]\melismaEnd e4~
  e8[ f] d4. ( e8 )
  %% 165
  e2.\fermata
}

\putMusic ten.lyrics \lyricmode {
  Lo -- bet den Herrn, al -- le, al -- le Hei -- den, al -- le Hei --   den, %% T 9
  lo -- bet den Herrn, al -- le Hei -- den, %% T 10
  lo -- bet, lo -- bet den Herrn, lo -- bet, lo -- bet, al -- le Hei -- den, al -- le Hei --  den, %% T 15
  lo -- bet den Herrn, lo -- bet den Herrn, %% T 18
  al -- le Hei -- den, al -- le Hei -- den, lo -- bet den Herrn, den Herrn, %% T 21
  al -- le, al -- le Hei -- den, al -- le, al -- le Hei -- den, %% T 23
  und prei -- set ihn, al -- le -- Völ -- ker,  al -- le -- Völ -- ker, %% T 30
  und prei -- set ihn, al -- le Völ -- ker, al -- le, al -- le Völ -- ker, %% 32
  und prei -- set, prei -- set ihn, und prei -- set prei -- set ihn, %% 34
  al -- le, al -- le Völ -- ker, prei -- set ihn, %% T 36 Mitte
  al -- le, al -- le, al -- le Völ -- ker, al -- le Völ -- ker, %% T 39
  und prei -- set, prei -- set ihn, und prei -- set ihn, al -- le, al -- le, al -- le Völ -- ker! %% T 42
  Lo --   bet den Herrn, al -- le -- Völ -- ker, %% T 45
  und prei -- set ihn, und prei -- set ihn, al -- le Völ -- ker, %% T 49
  und prei -- set ihn, al -- le, al -- le Völ -- ker, %% T 52 Anfang
  al -- le Völ -- ker, al -- le Völ -- ker, al -- le, al -- le Völ -- ker, %% T 54
  prei -- set ihn, al -- le, al -- le Völ -- ker, %% T 56
  und prei -- set ihn, al -- le Völ -- ker! %% T 58 Anfang
  Denn sei -- ne Gna -- de und Wahr -- heit wal -- tet ü -- ber uns, %% T 63
  denn sei -- ne Gna -- de und Wahr -- heit wal -- tet ü -- ber uns, %% T 69
  denn sei -- ne Gna -- de und Wahr -- heit %% T 72
  wal -- tet ü -- ber uns in E -- wig -- keit, %% T 77
  sei -- ne Gna -- de und Wahr -- heit wal -- et ü -- ber uns in E -- wig -- keit, in -- E -- wig -- keit, %% T 81
  sei -- ne Gna -- de und Wahr -- heit wal -- tet ü -- ber -- uns in E -- wig -- keit, %% T 84
  in E -- wig -- keit, sei -- ne Gna -- de und Wahr -- heit wal -- tet ü -- ber uns, %% T 87
  wal -- tet ü -- ber uns in E -- wig -- keit, %% T 89 Anfang
  wal -- tet ü -- ber uns in E -- wig -- keit, %% T 91
  in E -- wig -- keit, denn sei -- ne Gna -- de und Wahr -- heit wal -- tet ü -- ber uns in E -- wig -- keit. %% T 98
  Hal -- le -- lu -- ja, Hal -- le -- lu -- ja, __ %% T 105
  Hal -- le -- lu -- ja, Hal -- le -- lu -- ja, Hal -- le -- lu -- ja, %% T 110
  Hal -- le -- lu -- ja, Hal -- le -- lu -- ja, Hal -- le -- lu -- ja, %% T 116
  Hal -- le -- lu -- ja, Hal -- le -- lu -- ja, Hal -- le -- lu -- ja, %% T 122
  Hal -- le -- lu -- ja, Hal -- le -- lu -- ja, %% T 125
  Hal -- le -- lu -- ja, Hal -- le -- lu -- ja, __ Hal -- le -- lu -- ja, %% T 135
  Hal -- le -- lu -- ja, Hal -- le -- lu -- ja, Hal -- le -- lu -- ja, %% T 140
  Hal -- le -- lu -- ja __ Hal -- le -- lu -- ja, %% T 147
  Hal -- le -- lu -- ja, Hal -- le -- lu -- ja, Hal -- le -- lu -- ja, %% T 154 Anfang
  Hal -- le -- lu -- ja, Hal -- le -- lu -- ja, %% T 157
  Hal -- le -- lu -- ja, Hal -- le -- lu -- ja, Hal -- le -- lu -- ja.
}

\putMusic bas.music \relative c {
  R\breve
  R\breve
  R\breve
  R\breve
  %% 5
  R\breve
  r4 g\melisma b d g b, c g'
  a bes8[ a]\melismaEnd g[ f] e[ d] e4 c'8[\melisma b] a[ g f e]
  f4\melismaEnd f g g c,\melisma d8[ e] f2~
  f4\melismaEnd e8[ d] e4 a f d g8([ a g f]
  %% 10
  e4 ) c e g c( d8[ c]) b4 a
  g4 r4 r2 r1
  r4 d ( f ) a d\melisma e8[ d] c4\melismaEnd b
  a\breve~
  a4\melisma b8[ c] b4 a gis\melismaEnd e fis gis
  %% 15
  a a r2 r4 c8([ b] a4) g
  fis\melisma a8[ g] fis4 e dis\melismaEnd b cis dis
  e e r8 e[\melisma d c] b4\melismaEnd g r8 c[\melisma d e]
  f4\melismaEnd d g g, c c r2
  r4 c' ( b ) a g g, b\melisma d
  %% 20
  g b, c g' a bes8[ a]\melismaEnd g[ f] e[ d]
  e4 c'8[\melisma b] a[ g f e] f4\melismaEnd f g g
  c,\melisma d8[ e] f2~ f4 e8[ d] e4 a
  fis\melismaEnd d e fis g g, r2
  R\breve
  %% 25
  R\breve
  R\breve
  R\breve
  R\breve
  r1 r2 r4 g'
  %% 30
  a8[\melisma bes a g] f[ g f e] d4\melismaEnd c8 d e4 d8[ e]
  f4\melisma g8[ a] bes2~ bes4\melismaEnd c8[ bes] a[ g] f[ e]
  f4 d g f e c r4 c4
  f8[\melisma g f e] d4 e8[ f] g[ a]\melismaEnd g[ f] e4 e
  a8[\melisma bes a g] f4 g8[ a] bes[ c]\melismaEnd bes[ a] g4 g~
  %% 35
  g f e a f d r2
  r4 g g8[\melisma a g f] e[ f e d] c4\melismaEnd d8 e
  f4 f f8[\melisma g f e] d[ e d c] b4\melismaEnd c8[ d]
  e4 e r4 e4 a8[\melisma c b a] g[ a g f]
  e4\melismaEnd d8 e fis4 e8[ fis] g4\melisma a8[ b] c2~
  %% 40
  c4 d8[ c] b[ a]\melismaEnd gis[ fis] gis4 e r4 e4
  f ( d g ) f8 e a4\melisma b8[ c] d[ c b a]
  b4\melismaEnd g c c, g' g, b g
  c c r4 e4 f\melisma g8[ f] e[ d]\melismaEnd c[ b]
  a4 b8[\melisma c] d[ e d c] b4 g c8[ d] e4~
  %% 45
  e\melismaEnd d8[ c] d2 c4 c8([ d] e4) c
  f f r2 r4 d8([ e] fis4) d
  g g, b\melisma d g b, d g
  b c8[ b]\melismaEnd a[ g] fis[ e] fis4 b8([ c] d4) d
  d\melisma e8[ d] c[ b a gis] a4\melismaEnd a, r4 a'4
  %% 50
  a8[\melisma b a g] fis[ g fis e] d4\melismaEnd e8 fis g4 g
  g8[\melisma a g fis] e[ fis e dis] e4\melismaEnd fis8[ g] a2~
  a4 g fis b e,8[\melisma dis e fis] g2~
  g4\melismaEnd f e a f d r8 d[\melisma e f]
  g4 a8[ g] f4\melismaEnd g8[ f] e[\melisma f e d] c[ d c b]
  %% 55
  a4 g8[ a] b4 a8[ b] c4 d8[ e] f2~
  f4\melismaEnd e8[ d] e4\melisma f8[ g] a[ b c a] d[ c]\melismaEnd b[ a]
  b4( g) r4 a8([ g] f4) d g2
  c, r4 c' a2 d,
  g1 g2 e
  %% 60
  cis1 d2 r2
  r2 d'4\melisma c b g c b
  a gis8[ a] b4 a gis e\melismaEnd a( g)
  f ( e ) f2 e r4 e
  a2 a, d1 (
  %% 65
  c ) c2 a
  fis1 g2 r2
  r2 g'4\melisma f e c f e
  d cis8[ d] e4 d cis a\melismaEnd d( c)
  bes ( a ) bes2 a r4 a'
  %% 70
  f ( e ) d ( f ) a,1
  a2 a' a1
  a,2 a'~ a4\melisma b8[ cis] d4 c
  bes1. a2~
  a g1 f2~
  %% 75
  f\melismaEnd e f e
  d4 ( c ) d ( g, ) c ( d ) e ( c )
  g'4. g,8 g2 r1
  R\breve
  R\breve
  %% 80
  R\breve
  r2 c4 d e ( c c' ) bes8 c
  a4 ( g ) a ( f ) e\melisma f8[ g] a[ g f e]
  d4\melismaEnd c d e f c f2~
  f e f r4 f
  %% 85
  f4. f,8 f2 r1
  r4 bes bes' bes bes ( a8 g ) a4 f
  f e8 ( d e4 ) c'2\melisma bes8[ a] bes4 g~
  g f8[ e] f4\melismaEnd d a'4. a,8 a4 a'
  a\breve~
  %% 90
  a\breve~
  a2 g g r4 f
  e\melisma a2 g4 f8[ e]\melismaEnd d4 d' c
  b\melisma a8[ g] fis4\melismaEnd e8[ fis] g2 r4 g,
  d'2 ( g4 ) b, c2 r4 d
  %% 95
  g,\melisma g'8[ fis] g4\melismaEnd g a g ( a ) b
  c ( g ) c c, d ( c ) d ( e )
  f c f2~ f4 e8[ d] e4 c
  g'2 g, c r2

  R2.
  %% 100
  R2.
  R2.
  R2.
  g'8[ a b a] g[ b]
  a4 d, d'~
  %% 105
  d c8[ b] c[ a]
  b([ c] d4) r4
  c,8[ d e d] c[ e]
  d4 g, g'~
  g fis8[ e] fis4
  %% 110
  g8[ a] b[ a] g[ b]
  a4 d,4 r4
  r4 a' fis
  dis e4 r4
  R2.
  %% 115
  R2.
  r4 d\melisma g~
  g f8[ e]\melismaEnd d[ f]
  e4 a, a'~
  a g8[ fis] e[ g]
  %% 120
  fis[\melisma g a g fis a]
  b[ c d c b d]
  e,[ fis g fis e g]
  a[ b c b a c]
  es,4\melismaEnd b e~
  %% 125
  e\melisma es8[ cis]\melismaEnd dis4
  e4 r4 r4
  R2.
  R2.
  R2.
  %% 130
  d8[ e fis e] d[ fis]
  e4 a, a'~
  a g8[ f] g[ e]
  f[\melisma e d e f d]
  a'4\melismaEnd a,8[ b c a]
  %% 135
  e'[ f g f] e[ g]
  f[\melisma g f e d f]
  e4\melismaEnd c( f)
  d( b) e
  a, a'8[ gis] a4~
  %% 140
  a gis2
  a4 r4 r4
  R2.
  a8[ b c b] a[ c]
  b4 e, e'~
  %% 145
  e\melisma d8[ c]\melismaEnd d[ b]
  c4 c, c'~
  c\melisma b8[ a]\melismaEnd b[ g]
  a4 a, a'~
  a\melisma g8[ f]\melismaEnd g[ e]
  %% 150
  f4 f, f'~
  f e8[ d] e[ c]
  d[ e f e] d[ f]
  b,4 g c
  f g g,
  %% 155
  c4 r4 r4
  r4 c' c,
  g' g4 r4
  r4 a a,
  e' e4 r4
  %% 160
  r4 f f,
  c' c4 r4
  r4 d a
  b ( g ) c
  f g g,
  %% 165
  c2.\fermata
}

\putMusic bas.lyrics \lyricmode {
  Lo -- bet den Herrn, al -- le, al -- le Hei -- den, al -- le, %% T 9 Mitte
  al -- le Hei -- den, lo -- bet, lo -- bet den Herrn, %% T 11
  al -- le, al -- le Hei -- den, al -- le Hei -- den %% T 15
  al -- le Hei -- den, al -- le Hei -- den, al -- le, al -- le al -- le Hei -- den, %% T 18
  al -- le Hei -- den, lo -- bet den Herrn, %% T 21
  al -- le, al -- le Hei -- den, al -- le Hei -- den, %% T 23
  und prei -- set ihn, al -- le Völ -- ker, al -- le Völ -- ker, al -- le Völ -- ker, %% T 32
  und prei --    set ihn, und prei -- set ihn, al -- le, al -- le Völ -- ker, %% T 35
  und prei -- set ihn, al -- le, al -- le Völ -- ker, %% T 38
  und prei -- set ihn, al -- le, al -- le -- Völ -- ker, %% T 40
  und prei -- set ihn, al -- le, al -- le Völ -- ker, al -- le Völ -- ker, %% T 43
  und prei -- set ihn, al -- le Völ -- ker, al -- le Völ -- ker, al -- le Völ -- ker. %% T 47 Anfang
  Lo -- bet den Herrn, al -- le Hei -- den, %% T 49
  und prei -- set ihn, al -- le, al -- le --   Völ -- ker, al -- le Völ -- ker, al -- le Völ -- ker, %% T 53
  al -- le Völ -- ker, prei -- set ihn, %% T 57 Anfang
  al -- le Völ -- ker! %% T 58
  Denn sei -- ne Gna -- de und Wahr -- heit wal -- tet ü -- ber uns, %% T 63
  denn sei -- ne Gna -- de und Wahr -- heit wal -- tet -- ü -- ber uns, %% T 69
  denn sei -- ne Gna -- de und Wahr -- heit wal -- tet, wal -- tet ü -- ber uns in E -- wig -- keit, %% T 77
  sei -- ne Gna -- de und Wahr -- heit wal -- tet ü -- ber uns in E -- wig -- keit, in E -- wig -- keit, %% T 85
  denn sei -- ne Gna -- de und Wahr -- heit wal -- tet ü -- ber uns in E -- wig -- keit, %% T 91
  in E -- wig -- keit, in E -- wig -- keit, in E -- wig -- keit, in E -- wig -- keit, %% T 95
  sei -- ne Gna -- de und Wahr -- heit wal -- tet ü -- ber -- uns in E -- wig -- keit. %% T98
  Hal -- le -- lu -- ja, Hal -- le -- lu -- ja, __ %% T 105
  Hal -- le -- lu -- ja, Hal -- le -- lu -- ja, Hal -- le -- lu -- ja, Hal -- le -- lu -- ja, %% T 113
  Hal -- le -- lu -- ja, Hal -- le -- lu -- ja -- Hal -- le -- lu -- ja, %% T 126
  Hal -- le -- lu -- ja, Hal -- le -- lu -- ja, __ Hal -- le -- lu -- ja, __ %% T 137
  Hal -- le -- lu -- ja, Hal -- le -- lu -- ja, %% t 141
  Hal -- le -- lu -- ja, Hal -- le -- lu -- ja, Hal -- le -- lu -- ja, %% T 148
  Hal -- le -- lu -- ja, Hal -- le -- lu -- ja, __ Hal -- le -- lu -- ja, %% 153
  Hal -- le -- lu -- ja, Hal -- le -- lu -- ja, Hal -- le -- lu -- ja, Hal -- le -- lu -- ja, Hal -- le -- lu -- ja, Hal -- le -- lu -- ja. %% T 165

}

\putMusic continuo \relative c {
  c2 d2\rest d4\rest g8 f e d c b
  c2 g'4 fis g2 a4 g~
  g f b,2 c d4\rest a
  d2 d4\rest e f2 d4\rest d
  %% 5
  g c, e g c2 d,4\rest a'
  e g, b d g b, c g'
  a bes8 a g f e d e4 c'8 b a g f e
  f4 f g g c, d8 e f2~
  f4 e8 d e4 a f d g8 a g f
  %% 10
  e4 c e g c d8 c b4 a
  g g b d g a8 g f4 e
  d d, f a d e8 d c4 b
  a\breve~
  a4 b8 c b4 a gis e fis gis
  %% 15
  a2 d,4\rest e c c'8 b a4 g
  fis a8 g fis4 e dis b cis dis
  e2 d8\rest e d c b4 g r8 c d e
  f4 d g g, c2 d4\rest d
  e c' b a g g, b d
  %% 20
  g b, c g' a bes8 a g f e d
  e4 c'8 b a g f e f4 f g g
  c, d8 e f2~ f4 e8 d e4 a
  fis d e fis g g, d'2\rest
  d4\rest g a e f d g e
  %% 25
  a a, d e8 f g4 b, c e~
  e d8 c b4 g c a d b
  e2~ e4 fis8 g fis4 d g d'
  e4. d8 c4. b8 a4 g8 a b4 a8 b
  c4 d8 e f2~ f4 e8 d e4 g,
  %% 30
  a4. g8 f4. e8 d4 c8 d e4 d8 e
  f4 g8 a bes2~ bes4 c8 bes a g f e
  f4 d g f e8 f e d c4 d8 e
  f g f e d4 e8 f g a g f e4 f8 g
  a8 bes a g f4 g8 a bes c bes a g2~
  %% 35
  g4 f e a f d d4\rest d4
  g, g'~ g8 a g f e f e d c4 d8 e
  f2~ f8 g f e d e d c b4 c8 d
  e4 a, e'4 e4 a8 c b a g a g f
  e4 d8 e fis4 e8 fis g4 a8 b c2~
  %% 40
  c4 d8 c b a gis fis gis4 e a e
  f d g e a b8 c d c b a
  b4 g c c, g' g,8 a b4 g
  c c, r4 e'4 f g8 f e d c b
  a4 b8 c d e d c b4 g c8 d e4~
  %% 45
  e d8 c d2 c8 b c d e4 c
  f2~ f8 g f e d c d e fis4 d
  g g, b d g b, d g
  b c8 b a g fis e fis4 b8 c d2~
  d4 e8 d c b a gis a4 a, a'2~
  %% 50
  a4. g8 fis4. e8 d4 e8 fis g2~
  g4. fis8 e4. dis8 e4 fis8 g a2~
  a4 g fis b e,8 dis e fis g2~
  g4 f e a f d2 e8 f
  g4 a8 g f4 g8 f e4. d8 c4. b8
  %% 55
  a4 g8 a b4 a8 b c4 d8 e f2~
  f4 e8 d e4 f8 g a b c a d c b a
  b4 g c8 b a g f4 d g2
  c, d4\rest c' a2 d,
  g1. e2
  %% 60
  cis1 d2 d2\rest
  d2\rest d'4 c b g c b
  a gis8 a b4 a gis e a g
  f e f2 e d4\rest e
  a2 a, d1
  %% 65
  c1. a2
  fis1 g2 d'2\rest
  d2\rest g4 f e c f e
  d cis8 d e4 d cis a d c
  bes a bes2 a d4\rest a'
  %% 70
  f e d f a,1~
  a2 a' a1~
  a2 a,~ a4 b8 cis d4 c
  bes1. a2~
  a g1 f2~
  %% 75
  f e f e
  d4 c d g c, d e c
  g'1 d'2\rest b'4 g
  c2 c,4 e g2 c4 e,
  f e d2 c4 d e c
  %% 80
  g'2 g, c d2\rest
  d2\rest c4 d e c c' bes8 c
  a4 g a f e f8 g a g f e
  d4 c d e f c f2~
  f e f d4\rest f
  %% 85
  f,1 d'4\rest f~ f8 es d c
  d4 bes bes'2~ bes4 a8 g a4 f~
  f e8 d e4 c'~ c bes8 a bes4 g~
  g f8 e f4 d a'2 a,~
  a\breve~
  %% 90
  a\breve~
  a2 g1 d'4\rest f,
  e a2 g4 f d d' c
  b2 fis g d'4\rest g,
  d'2 g4 b, c2 d4\rest d
  %% 95
  g, g'8 fis g4 g, a g a b
  c g c, c' d c d e
  f c f2~ f4 e8 d e4 c
  g'2 g, c, d'2\rest

  \clef violin
  %% 100
  << {c''8 d e d c e d2 g4~ g4 f8 e f4} \\ {R2. g,8 a b a g b a4 d, d'} >>
  \clef bass c,8 d e d c e
  g, a b a g b
  a4 d, d'~
  %% 105
  d c8 b c a
  b a b c d b
  << {c} \\ {c, d e d c e} >>
  d4 g, g'~
  g fis8 e fis4
  %% 110
  g8 a b a g b
  a4 d, d'~
  d << {c} \\ {a} >> fis
  dis e c'
  a b8 a g fis
  %% 115
  e4 fis2
  g4 d g~
  g f8 e d e
  e4 a, a'~
  a g8 fis e g
  %% 120
  fis4 a fis
  b d b
  e, g e
  a c a
  dis, b e~
  %% 125
  e dis2
  e4 \clef violin g'8 fis e g
  fis4 b, b'~
  b a8 g fis a
  << {g} \\ {g, a b a g b} >>
  %% 130
  \clef bass d, e fis e d fis
  e4 a, a'~
  a g8 f g e
  f e d e f d
  a'4 a,8 b c a
  %% 135
  e' f g f e g
  f g f e d f
  e4 c f
  d b e
  a, a'8 gis a4~
  %% 140
  a gis2
  a4 c a
  b d b
  << {a} \\ {a,8 b c b a c} >>
  b4 e, e'~
  %% 145
  e d8 c d b
  c4 c, c''~
  c b8 a b g
  a4 a, a'~
  a g8 f g e
  %% 150
  f4 f, f'~
  f e8 d e c
  d4 f d
  b g c
  f g g,
  %% 155
  c4 r2
  r4 c' c,
  g' g,4 r4
  r4 a' a,
  e' e,4 r4
  %% 160
  r4 f' f,
  c' c,4 r4
  r4 d' a
  b g c
  f g g,
  %% 165
  c,2.\fermata
}

\lalilyTest

