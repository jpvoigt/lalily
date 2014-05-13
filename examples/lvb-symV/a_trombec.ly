\version "2.18.0"

trombec = {
  \set Staff.instrumentName = "Corni in Es."
  \set Staff.midiInstrument = "english horn"
  \clef treble
  \key c \major
  \time 2/4
  \repeat volta 2 {
    R2 r2^\fermata R2 R2 r2^\fermata R2 R2 R2 R2 R2 R2 R2 R2 R2 R2 R2 R2
    r8 <g g'>8_\markup { \italic " p cresc." }[ <g g'> <g g'>] <c' c''>4\f r <c' c''> r <g g'> r4^\fermata
    R2 R2 r2^\fermata R2 R2 R2 R2 R2 R2 R2 R2 R2 R2 R2 R2 R2 R2
    R2 R2 R2 R2 <c' c''>4\f r <c' c''>\f r R2 R2 r4 <c' c''>8 <c' c''> <g g'>4 r R2 R2
    r4 <c' c''>8 <c' c''> <c' c''>2\ff~ <c' c''>~ <c' c''>~ <c' c''> <c' c''>4 r R2 R2
    R2 R2 R2 R2 R2 R2 R2 R2 R2 R2 R2 R2 R2 R2 R2 R2 R2 R2 R2 R2 R2
    R2 R2 R2 R2 R2 R2 R2 R2 R2 R2 R2 R2 R2 R2
    << { f''2~ f''4 } \\ { f''2\ff~ f''4 } >> r4 R2 R2 <g g'>4\f r <c' c''> r R2 R2 << { f''2~ f''2 g''4 } \\ { f''2\ff~ f''2 g''4 } >> <g' g''>4
    R2 r4 <g g'> r <c' c''> R2 R2 R2 R2 R2 R2 R2 R2 R2 R2 R2 R2 R2 R2 R2 R2 R2
  }

  R2 R2 R2 r2^\fermata R2 R2 R2 R2 R2 R2 R2 R2 R2 R2 R2 R2 R2 R2 R2 R2 R2 R2 R2 R2 R2 R2 R2 R2
  R2 R2 R2 R2 R2 <g g'>4\pp r4 R2 << { c'4 } \\ { c'4 } >> r4 R2 << { c'4 } \\ { c'4 } >> r4 R2
  <g g'>4 r R2 <g g'>4 r R2 <c' c''>2\f~ <c' c''>2 <c' c''>4 r r8 <e' e''>8[ <e' e''> <e' e''>]
  <e' e''>2~ <e' e''>2 <e' e''>4 r
  \set crescendoText = \markup { \italic "piu f" } \set crescendoSpanner = #'text
  r8 <g' g''>\<[ <g' g''> <g' g''>] <g' g''>4 r r4 <g' g''>8 <g' g''> <g' g''>4\! r4 << { d''4 } \\ { d''4 } >> r4
  R2 R2 r4 << { c'8 c' c'4 } \\ { c'8 c' c'4 } >> r4 R2 R2 r4 << { d''8 d'' } \\ { d''8 d'' } >> <g' d''>4 r R2 R2
  R2 R2 R2 R2 r4 <g g'>8 <g g'> <c' c''>4 r4 << { c'2 c'2 } \\ { c'2 c'2 } >> R2 R2 << { c'2 c'2 } \\ { c'2 c'2 } >>
  R2 R2 R2 R2 R2 R2 R2 R2 R2 R2 R2 R2 R2 R2 R2 R2 R2 R2 R2 R2 R2 R2 R2 R2
  R2 R2 r8 << { d''8[ d'' d''] d''2 d''2 d''2~ d''2 } \\ { d''8\ff[ d'' d''] d''2 d''2 d''2~ d''2 } >>
  R2 R2 R2 R2 R2 R2 R2 R2 R2 R2 R2 R2 R2 R2 R2 r8 <g' g''>8\ff[ <g' g''> <g' g''>]
  <g' g''>2^\fermata r8 <g' g''>[ <g' g''> <g' g''>]  <g' g''>2~ <g' g''>2^\fermata R2 R2 R2 R2
  R2 R2 R2 R2 R2 R2 R2 R2 R2 R2 <c' c''>4\p r4

  \cadenzaOn <g g'>4\f s4 s2 s2 r4^\fermata s2 s4 \cadenzaOff \bar "|"
  R2 R2 R2 R2 R2 R2 R2 R2 R2 <c' c''>4\f r R2 <c' c''>4 r
  R2 R2 R2 R2 R2 R2 <c' c''>4\f r <c' c''>\f r R2 R2 r4 <c' c''>8[ <c' c''>] <g g'>4 r
  R2 R2 r4 <c' c''>8[ <c' c''>] <c' c''>2\ff~ <c' c''>~ <c' c''>~ <c' c''> <c' c''>4 r R2 <g g'>4 r R2 R2 R2
  R2 R2 R2 R2 R2 R2 R2 R2 R2 R2 R2 R2 R2 R2 R2 R2 R2 R2 R2 R2 R2 R2 R2 R2 R2 R2 R2 R2 R2 R2 R2 R2
  R2 R2 R2 R2 R2 R2 R2 R2 <g' d''>2\ff~ <g' d''>4 <g' d''> <e' c''>4 r <g g'> r <c' c''> r
  <c' c''>4 r <c' c''> r <g g'> r <g' d''>2~ <g' d''>2 <c'' e''>4 <c' c''> r <g g'> r <c' c''> r <c' c''> r <c'' e''> r <g' d''>
  <e' c''>4 r4 R2 R2 r8 <g g'>8[ <g g'> <g g'>] <c' c''>4 r R2 R2 r8 <g g'>[ <g g'> <g g'>] <c' c''>4 r
  r8 <g g'>[ <g g'> <g g'>] <c' c''>4 r r8 <g g'>[ <g g'> <g g'>] <c' c''>2:8
  <c' c''>2~ <c' c''>2:8 <c' c''>2~ <c' c''>2:8 <c' c''>2~ <c' c''>2:8 <c' c''>2 R2 R2 R2 R2 R2 R2
  R2 R2 r8 <c' c''>8\ff[ <c' c''> <c' c''>] <c' c''>2~ <c' c''>~ <c' c''>~ <c' c''> <c' c''>4 r4
  r8 <c' c''>8[ <c' c''> <c' c''>] <c' c''>4 r4 R2 <g g'>4 r4 R2 R2 R2 <g g'>4 r4
  R2 R2 R2 R2 R2 R2 R2 R2 R2 R2 R2 r4 <g g'> <g g'> <g g'> <g g'> <g g'> <g g'> <c' c''> <c' c''> <c' c''>
  <c' c''>4 <g g'> <g g'> <g g'> <g g'> <g g'> <c' c''>2~ <c' c''> <c' c''>~ <c' c''> <c' c''>~ <c' c''> <c' c''>~ <c' c''>
  <g g'>2 <g g'> <c' c''>4\staccato r <c' c''>\staccato r <c' c''>\staccato r <c' c''>\staccato r
  <g g'>4\staccato r <g g'>\staccato r <c' c''>2
  <c' c''>4 <c' c''> <c' c''>2 <c' c''>4 <c' c''> <c' c''>2 <c' c''>4 <c' c''> <c' c''>2
  <g g'>4 <g g'> <g g'>2 <g g'>4 <g g'> <g g'>2 <g g'>4 <g g'> <g g'>2 <g g'>4 <g g'> <c' g'>4 r4 R2 R2
  <c' c''>4 <c' c''> R2 <c' c''>4 <c' c''> r4 <g g'> r <g g'> R2 R2
  <c' c''>4 <c' c''> <c' c''> <c' c''> <c' c''> <c' c''> <c' c''> <c' c''> r4 <g g'> r <g g'> <c' c''> r
  R2 R2 R2 R2 r8 <g g'>8[ <g g'> <g g'>] <c' c''>4 r4
  R2 R2 r8 <g g'>[ <g g'> <g g'>] <g g'>2\ff^\fermata r8 <g g'>[ <g g'> <g g'>] <g g'>2~ <g g'>2^\fermata
  R2 R2 R2 R2 R2 R2 R2 R2 r8 <g g'>\ff[ <g g'> <g g'>] <c' c''>4 r r8 <g g'>[ <g g'> <g g'>] <c' c''>4 r
  r8 <g g'>[ <g g'> <g g'>] <c' c''>4 <g g'> <c' c''> <g g'> <c' c''> <g g'> <c' c''> <g g'>
  <c' c''>4 r <g g'> r << { c'4 } \\ { c'4 } >> r4 \bar "|."
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
