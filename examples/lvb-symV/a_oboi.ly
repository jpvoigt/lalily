\version "2.18.0"

oboi = {
  \set Staff.instrumentName = "Oboi."
  \set Staff.midiInstrument = "oboe"
  \clef treble
  \key c \minor
  \time 2/4
  \repeat volta 2 {
    R2 r2^\fermata R2 R2 r2^\fermata R2 R2 R2 R2 R2 R2 R2 R2
    R2 R2 R2 R2 r8 <d'' g''>8_\markup { \italic "p cresc." }[ <d'' g''> <d'' f''>]
    <c'' ees''>4\f r <c'' fis''> r <b' g''> r^\fermata
    r8 << { aes''8[ aes'' aes''] f''2~ f''2^\fermata } \\ { aes''8\ff[ aes'' aes''] f''2~ f''2 } >>
    R2 R2 R2 R2 R2 R2 R2 << { f''8\rest f''8\p[ f'' f''] ees''4 e''4\rest } \\ { R2 R2 } >> R2 R2
    <ees'' g''>2_\markup { \italic "cresc." }~ <ees'' g''>4 r4 <f'' aes''>4\sf r4
    <e'' g''>4\sf r <f'' aes''>\sf r <g'' bes''>\sf r <f'' aes''>\sf r <f'' aes''>\sf r
    <ees'' g''>2\f~ <ees'' g''>~ <ees'' g''>~ <ees'' g''> <f'' g''>2~ <f'' g''>~ <f'' g''>~ <f'' g''>4 <ees'' g''>4
    <ees'' ges''>2\ff~ <ees'' ges''>~ <ees'' ges''>~ <ees'' ges''> <ees'' ges''>4 r R2 <bes' f''>4 r
    R2 R2 R2 R2 R2 R2 R2 R2 R2 R2 R2 R2 R2 R2 R2 R2 R2 R2 R2 R2 R2 R2 R2 R2 R2
    << { \crescTextCresc c'''2\<~ c'''2 bes''2~ bes''2 a''2~ a''~ a''~ a''~ a''4 a''4 a'' a''\!} \\ { R2 R2 R2 R2 R2 R2 R2 R2 R2 R2 } >>
    <f'' aes''!>2\ff~ <f'' aes''>4 <f'' aes''> <ees'' g''>4 r << { aes''4 } \\ { aes''4 } >> r <ees'' bes''>4 r <ees'' c'''> r <ees'' g''> r
    <d'' f''> r <f'' aes''>2\ff~ <f'' aes''> <ees'' g''>4 <ees'' g''> r <d'' f''> r <ees'' g''> r <ees'' f''> r <ees'' g''> r <d'' f''>
    <ees'' g''>8 <<
      {
        g''8[ g'' g''] ees'' bes''[ bes'' bes''] g'' ees''[ ees'' ees''] bes'2~ bes'8 g''8[ g'' g'']
        ees''8 bes''[ bes'' bes''] g'' ees''[ ees'' ees''] bes'2~ bes'4
      } \\
      {
        g''8[ g'' g''] ees'' bes''[ bes'' bes''] g'' ees''[ ees'' ees''] bes'2~ bes'8 g''8[ g'' g'']
        ees'' bes''[ bes'' bes''] g'' ees''[ ees'' ees''] bes'2~ bes'4
      }
    >> r4
    r8 <d'' f''>[ <d'' f''> <d'' f''>] <ees'' g''>4 r4 r8 <f'' bes''>8[ <f'' bes''> <f'' bes''>] <ees'' g''>4 r4 R2 R2
  }

  R2 R2 R2 r2^\fermata R2 R2 R2 R2 R2 R2 R2 R2 R2 R2 R2 R2 R2 R2 R2 R2 R2 R2 R2 R2 R2 R2 R2 R2
  << { f''8\rest fis''8\p[ fis'' a''] g''2 fis''8[ fis'' fis'' a''] g''2 fis''2 g''2~ g''8 f''8\rest f''4\rest } \\ { R2 R2 R2 R2 R2 R2 R2 } >>
  <c'' fis''>4 r R2 <c'' fis''>4 r R2 <g' g''>4 r R2 <d'' g''>4 r
  << { f''8\rest g''8_\markup { \italic "cresc." }[ a'' bes''] } \\ { R2 } >> <fis'' a''>2\f~ <fis'' a''>2 <fis'' a''>4 r
  r8 <g'' bes''>8[ <g'' bes''> <g'' bes''>] <g'' bes''>2~ <g'' bes''>2 <g'' bes''>4 r
  \set crescendoText = \markup { \italic "piu f" } \set crescendoSpanner = #'text
  r8 <g'' bes''>8\<[ <g'' bes''> <g'' bes''>] <g'' bes''>4 r r4 <g'' bes''>8 <g'' bes''> <g'' bes''>4 <d'' a''>8 <d'' a''> <d'' a''>4\! r4
  R2 R2 R2 R2 R2 r4 << { fis''8[ fis''] fis''4 } \\ { fis''8[ fis''] fis''4 } >> <fis'' a''>8 <fis'' a''> <g'' b''>4 r R2 R2
  R2 R2 R2 r4 << { f''8 f'' f''4 } \\ { f''8 f'' f''4 } >> r4 r8 <c'' c'''>8\ff[ <c'' c'''> <c'' c'''>]
  << { f''2 g'' } \\ { f''2 g'' } >> R2 R2 <f' f''>2 <g' g''>2 R2 R2 << { f''2 ges'' } \\ { f''2 ges'' } >> R2 R2 << { a''2 bes'' } \\ { a''2 bes'' } >>
  R2 R2 R2 R2 R2 R2 R2 << { a''2_\markup { \italic "sempre piu p" } } \\ { R2 } >> R2 << { a''2 } \\ { R2 } >> R2
  << { a''2\pp } \\ { R2 } >> R2 << { a''2 } \\ { R2 } >> R2 << { a''2 } \\ { R2 } >> R2 << { a''2 } \\ { R2 } >>
  r8 <d'' d'''>8\ff[ <d'' d'''> <d'' d'''>] <b' b''>2 <c'' c'''> <a' a''>~ <a' a''>
  << { aes''!2\pp } \\ { R2 } >> R2 << { aes''2 } \\ { R2 } >> R2 << { aes''2 } \\ { R2 } >> R2 << { aes''2 } \\ { R2 } >> R2
  r8 <<
    { aes''8[ aes'' aes''] f''2~ f''8 aes''8[ aes'' aes''] f''2~ f''8 aes''[ aes'' aes''] f'' aes''[ aes'' aes''] f''[ aes'' aes'' aes''] } \\
    { aes''8\ff[ aes'' aes''] f''2~ f''8 aes''8[ aes'' aes''] f''2~ f''8 aes''[ aes'' aes''] f'' aes''[ aes'' aes''] f''[ aes'' aes'' aes''] }
  >>
  <f' f''>8 <g' g''>[ <g' g''> <g' g''>] <ees'' g''>2^\fermata r8 <f'' g''>[ <f'' g''> <f'' g''>] <d'' g''>2~ <d'' g''>2^\fermata
  R2 <<
    { c''2\p~ c''2~ c''2~ c''2 d''2~ d''2~ d''2~ d''2 ees''2( d'' c'' b'4 c''8 d'') ees''2_\markup { \italic "cresc." }( c''2) } \\
    { R2 R2 R2 R2 R2 R2 R2 R2 R2 R2 R2 R2 R2 R2 }
  >>

  <<
    {
      \cadenzaOn g''2\f^\fermata\> \set fontSize = #-2 f''4^\markup { "Adagio" }_( ees''\! d''2 ees''16[ d'' c'' d'']
      f''4 ees'' d''4^\fermata) \set fontSize = #0 \cadenzaOff
    } \\
    { \cadenzaOn s2 s2 s2 r2_\fermata s2 \cadenzaOff }
  >> \bar "|"
  R2 <d'' f''>2\p~ <d'' f''>~ <d'' f''> <c'' ees''>4 r4 R2
  << { b''2_\markup { \italic "cresc." }~ b''2 c'''4\f g''4\rest } \\ { R2 R2 R2 } >> R2 <f'' aes''>4\f
  r4 r8 <ees'' g''>[ <ees'' g''> <ees'' g''>] <ees'' g''>4 r <f'' aes''>\sf r <e'' g''>\sf r <f'' aes''>\sf r
  <g'' bes''>4\sf r <f'' aes''>\sf r <f'' aes''>\sf r <ees'' g''>2\f~ <ees'' g''>~ <ees'' g''>~ <ees'' g''>
  <f'' g''>2~ <f'' g''>2~ <f'' g''>2~ <f'' g''>4 <ees'' g''> <fis'' a''>2\ff~ <fis'' a''>~ <fis'' a''>~ <fis'' a''>
  <fis'' a''>4 r R2 <d'' g''>4 r R2 R2 R2
  R2 R2 R2 R2 R2 \set doubleSlurs = ##t <c'' e''>2\p( <d'' f''>) <b' f''>2( <c'' e''>) R2 R2 R2 R2
  <c'' e''>2( <d'' f''>) <b' f''>2( <c'' e''>) \set doubleSlurs = ##f
  R2 R2 R2 R2 R2 R2 R2 R2 R2 R2 R2 R2 R2 R2 R2 R2 R2 R2 R2 R2 R2 R2 R2
  <g'' b''>2\ff~ <g'' b''>4 <g'' d'''> <g'' c'''>4 r <f'' g''> r <c'' g''> r
  <d'' c'''>4 r <e'' c'''> r <d'' b''> r <f'' d'''>2~ <f'' d'''>2 <e'' c'''>4 <e'' c'''> r4 <d'' b''> r <e'' c'''> r <d'' c'''>
  r4 <e'' c'''> r <d'' b''>
  <e'' c'''>8 <<
    { e''8[ e'' e''] c'' g''[ g'' g''] e'' c''[ c'' c''] g'2~ g'8 e''[ e'' e''] c'' g''[ g'' g''] e'' c''[ c'' c''] g' } \\
    { e''8[ e'' e''] c'' g''[ g'' g''] e'' c''[ c'' c''] g'2~ g'8 e''[ e'' e''] c'' g''[ g'' g''] e'' c''[ c'' c''] g' }
  >>
  <b' d''>8[ <b' d''> <b' d''>] <c'' e''>4 r r8 <b' d''>[ <b' d''> <b' d''>] <c'' e''>4 r r8 <d'' g''>[ <d'' g''> <d'' g''>]
  <e'' g''>8 <c'' c'''>[ <c'' c'''> <c'' c'''>] <c'' c'''>2~ <c'' c'''>2:8 <c'' c'''>2~ <c'' c'''>2:8 <c'' c'''>2~ <c'' c'''>2:8
  <c'' c'''>2 << { des'''4( aes'')~ aes''2~ aes''2~ aes''2 } \\ { des''2~ des''2~ des''2~ des''2 } >> <des'' aes''>4 r R2
  R2 R2 r8 <ees'' a''>8\ff[ <ees'' a''> <ees'' a''>] <ees'' a''>2~ <ees'' a''>~ <ees'' a''>~ <ees'' a''> <ees'' a''>4 r
  r8 << { c''8[ c'' c''] ees''4 } \\ { c''8[ c'' c''] ees''4 } >> r4 R2 R2 R2 <f'' b''>2~ <f'' b''> R2 R2 <f'' b''>2~ <f'' b''>
  R2 R2 R2 R2 <c'' c'''>4 <d'' d'''> <bes' bes''> <c'' c'''> <aes' aes''> <bes' bes''>
  <g' g''>4 <<
    { aes''4 f'' ees'' d'' ees'' f'' ees'' f'' g'' aes'' g'' aes'' } \\
    { aes''4 f'' ees'' d'' ees'' f'' ees'' f'' g'' aes'' g'' aes'' }
  >> <b' f''>4 <c'' f''> <b' f''> <c'' f''> <d'' f''>
  <c'' ees''>2~ <c'' ees''> <c'' f''>~ <c'' f''> <c'' g''>~ <c'' g''> <c'' aes''>( <c'' f''>) <b' f''> <d'' g''>
  <ees'' g''>4\staccato r <ees'' c'''>\staccato r <aes'' c'''>\staccato r <aes'' c'''>\staccato r <g'' c'''>\staccato r
  <g'' b''>\staccato r <c'' c'''>2\ff <c'' c'''>4\staccato <c'' c'''>\staccato <c'' c'''>2 R2 R2
  <c'' c'''>4\staccato <c'' c'''>\staccato <c'' c'''>2
  R2 R2 <g' g''>4\staccato <g' g''>\staccato <g' g''>2 R2 R2 <g' g''>4\staccato <g' g''>\staccato <g' g''>2
  <aes'' c'''>4( <g'' bes''> <f'' aes''> <ees'' g''>) R2 <f'' aes''>4\sf( <ees'' g''>) <f'' c'''>( <ees'' c'''>)
  r4 <ees'' c'''> r <d'' b''> R2 R2 R2 <f'' aes''>4( <ees'' g''>) R2 <f'' c'''>4( <ees'' c'''>)
  r4 <ees'' c'''> r <d'' b''> <ees'' c'''>4 r r8 <d'' b''>[ <d'' b''> <d'' b''>]
  <d'' b''>2~ <d'' b''>4( <ees'' c'''> <f'' d'''> <d'' b''> <ees'' c'''> <f'' d'''>) <ees'' c'''> r4
  r8 << { g''8[ g'' g''] g''2:8 g''2:8 } \\ { g''8[ g'' g''] g''2:8 g''2:8 } >> <ees'' g''>2\ff^\fermata
  r8 <f'' g''>[ <f'' g''> <f'' g''>] <d'' g''>2~ <d'' g''>2^\fermata R2 R2 R2
  << { f''4\rest aes''4\p( g''2) } \\ { R2 R2 } >> R2 R2
  << { f''4\rest aes''4^\p( g''8) } \\ { R2 f'8\rest } >> <d'' g''>8[ <d'' g''> <d'' g''>] <ees'' g''>4 r
  r8 <d'' b''>[ <d'' b''> <d'' b''>] <ees'' c'''>4 r r8 <d'' b''>[ <d'' b''> <d'' b''>]
  <ees'' c'''>4 <d'' b''> <ees'' c'''> <d'' b''> <ees'' c'''> <d'' b''> <ees'' c'''> <d'' b''>
  <ees'' c'''>4 r <d'' b''> r <ees'' c'''> r \bar "|."
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
