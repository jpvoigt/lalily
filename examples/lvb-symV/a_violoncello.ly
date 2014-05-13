\version "2.18.0"

violoncello = {
  \set Staff.instrumentName = "Violoncello."
  \set Staff.midiInstrument = "cello"
  \clef bass
  \key c \minor
  \time 2/4
  \repeat volta 2 {
    r8 g,8\ff[ g, g,] ees,2^\fermata r8 f,[ f, f,] d,2~ d,2^\fermata R2 c'2\p~ c'2~ c'2~ c'2 b2~ b2~ b2~ b2
    c'2 b c'2 b8_\markup { \italic "cresc." }[ b b b] c'4\f r aes, r g, r4^\fermata
    r8 aes,8\ff[ aes, aes,] f,2~ f,2^\fermata R2 R2 R2 r8 g,8\p[ g, g,] c4 r R2 R2 r8 g,\p[ g, g,] c4 r
    r8 \crescTextCresc c8\<[ c c] c4 r r8 c8[ c c] c4 r c4\!\sf r4
    c4\sf r c\sf r c\sf r c\sf r c\sf r c2\f~ c2~ c2~ c8[ c c c] g,2~ g,2~ g,2~ g,8[ g, c c] c2:8\ff c2:8 c2:8 c2:8 c4 r R2 d4 r
    R2 R2 R2 R2 R2 R2 r8 bes,8\p[ bes, bes,] ees4 r R2 R2 r8 bes,8[ bes, bes,] ees4 r R2 R2 r8 bes,8[ bes, bes,] ees4 r
    R2 R2 r8 c8[ c c] f4 r R2 R2 r8 ees8[ ees ees] aes4 r \clef tenor ees'4( f' \crescTextCresc ges'\< f' ees' f')
    ges'( f' ees' f') ges'( f' ees' f') ges'( f' ees' f' ges' f' ges' c'\!)
    \clef bass d'2\ff~ d'4 d ees4 r f r g r aes r bes r bes, r d4\ff f d f ees g f aes g bes aes c' bes bes, bes bes,
    ees2~ ees2~ ees2~ ees8 bes,8[ bes, bes,] ees2~ ees2~ ees2~ ees8 bes,8[ bes, bes,]
    ees4 r r8 bes,8[ bes, bes,] ees4 r r8 bes,8[ bes, bes,] ees4 r R2 R2
  }

  R2 r8 des,8\ff[ des, des,] c,2~ c,2^\fermata
  R2 \clef tenor c'2\p~ c'2~ c'8 des'8[ des' des'] c'2~ c'2~ c'2~ c'8 des'8[ des' des'] c'2~ c'2~ c'2~ c'2~ c'2~ \crescTextCresc
  c'2\<( d'4 ees' f' fis') g'8\!\p[ g' g' f'!] ees'2 d'8[ g' g' f'] ees'2 d'8[ g' g' f']
  \crescTextCresc ees'8\<[ ees' ees' d'] c'[ c' c' bes] a[ a a g] fis4\!\p r4
  \clef bass r8 bes8[ bes c'] d'2~ d'8[ bes bes c'] d'[ d' d' c'] bes[ bes bes a] g[ g g f] ees[ ees ees d]
  c8[ c' c' bes] a[ a a g] fis[ fis fis ees] d[ d d c] bes,[ d' d' c'] bes_\markup { \italic "cresc." }[ bes bes a]
  g[ f ees d] <c, c>2:8\f <c, c>2:8\f <c, c>4 r r8 cis8[ cis cis] cis2:8 cis2:8 cis4 r
  \set crescendoText = \markup { \italic "piu f" } \set crescendoSpanner = #'text
  r8 d8\<[ d d] d4 r r4 e8 e e4 fis8 fis fis4\! r4 R2 R2 r4 c'4~ c'4 a4\staccato fis\staccato d\staccato
  c4\staccato a,\staccato fis,\staccato d,\staccato g,4 r R2 R2
  r4 f'!4~ f'4 d'\staccato b\staccato g\staccato f\staccato d\staccato b,\staccato g,\staccato c4 r
  R2 R2 g,2 aes, R2 R2 g,2 aes, R2 R2 bes,2 c R2 R2
  des!2_\markup { \italic "dimin." } R2 ees2 R2 f2 R2 fis,2\p R2 fis,2_\markup { \italic "sempre piu p" } R2
  fis,2 R2 fis,2\pp R2 fis,2 R2
  fis,2 R2 fis,8\ff d[ d d] b,2 c2 a,2~ a,2 R2 b,2\pp R2 b,2 R2 b,2 R2 b,8\ff aes,[ aes, aes,] f,2~ f,8 aes,8[ aes, aes,]
  f,2~ f,8 aes,[ aes, aes,] f, aes,[ aes, aes,] f, aes,[ aes, aes,] f,[ aes, aes, aes,] f, g,[ g, g,]
  ees,2^\fermata r8 f,[ f, f,] d,2~ d,2^\fermata R2 c4\p^\markup { \italic "pizz." } r4 R2 c4 r4
  R2 b,4 r R2 b,4 r R2 c4 r b, r c r g, r c4_\markup { \italic "cresc." } r aes, r

  \cadenzaOn g,4\ff^\markup { "arco." } s4 s2 s2 r4^\fermata s2 s4 \cadenzaOff \bar "|" R2 R2 R2 r8 g,8\p[ g, g,]
  c8 aes_\markup { \italic "cresc." }[ aes aes] f d[ d d] b, aes,[ aes, aes,] g, g[ g g] c2:8\f c2~ c2:8 c2~
  c2:8 c4\sf r c\sf r c\sf r c\sf r c\sf r c\sf r c2\f~ c2~ c2~ c2:8 g,2~ g,2~ g,2~ g,8[ g, c c]
  c8\ff ees'[ ees' ees'] c' a[ a a] fis ees[ ees ees] c a,[ a, a,] c4 r R2 b,4 r R2 R2 R2
  R2 R2 R2 r8 g,8\p[ g, g,] c4 r4 R2 R2 R2 R2 R2 R2 r8 g,8[ g, g,] c4 r4 R2 R2 R2 R2
  R2 R2 r8 c8[ c c] f4 r4 R2 R2 r8 d8[ d d] g4 r4
  \clef tenor f'4( g' \crescTextCresc aes'!\< g' f' g') aes'( g' f' g') aes'4( g' f' g') aes'( g' fis' g')
  a'( g' fis' g') a'( g' fis' g' fis' g' fis' g'\!) \clef bass d'2\ff~ d'4 b4 c'4 r d' r e' r f' r g' r g r
  b,4 d b, d c e d f e g f a g g, g g,
  c2~ c~ c~ c8 g,[ g, g,] c2~ c~ c~ c8 g,[ g, g,] c4 r r8 g,[ g, g,] c4 r r8 g,[ g, g,]
  c2\sf~ c2:8 aes,!2\sf~ aes,2:8 e2\sf~ e2:8 f2\sf~ f2:8 f,8\ff f[ f f] f2:8 f2:8 f2:8 f4 r R2
  R2 R2 r8 fis,\ff[ fis, fis,] fis, fis[ fis fis] fis2:8 fis2:8 fis2:8 fis4 r R2 R2
  \clef tenor r8 g'8\f[ g' g'] ees'2 f' d'2~ d'8[ g' g' g'] ees'2 f' d'2~ d'8[ g' g' g']
  ees'4 f' d' ees' c' d' bes c' \clef bass aes bes g aes f g ees f
  d4 c b, c d c d e f e f g aes g a b c'2~ c' aes!~ aes ees~ ees f ees d f ees4\staccato r c\staccato r
  f4\staccato r d\staccato r g\staccato r g,\staccato r c\staccato r R2 R2 ees,4\staccato f,\staccato g,( ees,) R2 R2
  g,4\staccato a,\staccato b,( g,) R2 R2 g,4\staccato a,\staccato b,( g,) R2 R2 R2 R2 f4\sf( c) R2 f4( c)
  g4 r g, r c'4( g aes ees f c) R2 f4( c) f( c) g4 r g, r c4 r R2 R2 R2 R2 r8 g,8[ g, g,] c8 g,[ g, g,]
  g,2:8 g,2:8 g,2:8 ees,2\ff^\fermata r8 f,[ f, f,] d,2~ d,2^\fermata R2
  <c, g,>2\pp~ <c, g,>~ <c, g,>~ <c, g,>~ <c, g,>~ <c, g,>~ <c, g,>~ <c, g,>8 g,8\ff[ g, g,] c4 r
  r8 g,[ g, g,] c4 r r8 g,[ g, g,] c4 g, c g, c g c' g c' r g, r <c, c> r \bar "|."
}

%{
/usr/bin/python: /home/jpv/lily2.18/lilypond/usr/lib/libz.so.1: no
version information available (required by /usr/bin/python) convert-ly
(GNU LilyPond) 2.18.2  convert-ly: »« wird verarbeitet... Anwenden der
Umwandlung: 2.11.2, 2.11.3, 2.11.5, 2.11.6, 2.11.10, 2.11.11, 2.11.13,
2.11.15, 2.11.23, 2.11.35, 2.11.38, 2.11.46, 2.11.48, 2.11.50,
2.11.51, 2.11.52, 2.11.53, 2.11.55, 2.11.57, 2.11.60, 2.11.61,
2.11.62, 2.11.64, 2.12.0, 2.12.3, 2.13.0, 2.13.1, 2.13.4, 2.13.10,
2.13.16, 2.13.18, 2.13.20, 2.13.27, 2.13.29, 2.13.31, 2.13.36,
2.13.39, 2.13.40, 2.13.42, 2.13.44, 2.13.46, 2.13.48, 2.13.51, 2.14.0,
2.15.7, 2.15.9, 2.15.10, 2.15.16, 2.15.17, 2.15.18, 2.15.19, 2.15.20,
2.15.25, 2.15.32, 2.15.39, 2.15.40, 2.15.42, 2.15.43, 2.16.0, 2.17.0,
2.17.4, 2.17.5, 2.17.6, 2.17.11, 2.17.14, 2.17.15, 2.17.18, 2.17.19,
2.17.20, 2.17.25, 2.17.27, 2.17.29, 2.17.97, 2.18.0
%}
