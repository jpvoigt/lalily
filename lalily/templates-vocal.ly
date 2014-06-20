%%%% This file is part of lalily, an extension to lilypond <http://www.lilypond.org/>.
%%%%
%%%% Copyright (C) 2011--2014 Jan-Peter Voigt <jp.voigt@gmx.de>
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

\version "2.18.2"

#(define lalily-relincl-tmp (ly:get-option 'relative-includes))
#(ly:set-option 'relative-includes #t)
\include "../lalily.ly"
#(ly:set-option 'relative-includes lalily-relincl-tmp)

\registerTemplate lalily.init.Voice.vocal
#(define-music-function (parser location piece options)(list? list?)
   (let ((deepdef (assoc-get 'deepdef options #{ \dynamicUp \autoBeamOff #}))
         (deepsym (assoc-get 'deepsym options 'init-vocal))
         (init-opts (assoc-get 'init-opts options '())))
     #{
       \callTemplate ##t lalily.init.Voice #'() #(assoc-set-all! init-opts `((deepdef . ,deepdef)(deepsym . ,deepsym)))
     #}))


\registerTemplate lalily.vocal
#(define-music-function (parser location piece options)(list? list?)
   (let ((init-opts (assoc-get 'init-opts options '() #f))
         (clef (assoc-get 'clef options "G" #f))
         (vocname (assoc-get 'vocname options #f #t))
         (staffname (assoc-get 'staffname options #f #f))
         (staff-mods (assoc-get 'staff-mods options #f #f))
         (voice-mods (assoc-get 'voice-mods options #f #f))
         ;(lyric-mods (assoc-get 'lyric-mods options #f #f))
         ;(repeats (assoc-get 'repeats options #f #f))
         (verses (assoc-get 'verses options #f #f)))
     (ly:message "-> ~A" options)
     (if (not (string? vocname))
         (let ((tmpname (glue-list piece "-")))
           (ly:input-warning location "using ~A as vocname!" tmpname)
           (set! vocname tmpname)
           ))
     (if (not (string? staffname)) (set! staffname (glue-list piece ":")))
     #{
       <<
         \new Staff = $staffname \with {
           $(if (ly:context-mod? staff-mods) staff-mods #{ \with {} #})
           \consists \editionEngraver $piece
         } \new Voice = $vocname \with {
           $(if (ly:context-mod? voice-mods) voice-mods #{ \with {} #})
         } <<
           \getMusicDeep #'meta
           { \callTemplate ##t lalily.init.Voice.vocal #'() #init-opts \clef $clef \getMusic music }
         >>
         $(if (list? verses)
              #{ \stackTemplate lyrics lyrics #(assoc-set! options 'lyric-voice vocname) #'verse #(map (lambda (v) (list v)) verses) #}
              #{ \callTemplate lyrics lyrics #(assoc-set! options 'lyric-voice vocname) #})
         %{
         % TODO repeats
         $(if (list? verses)
              (make-music 'SimultaneousMusic
                'elements (map (lambda (v)
                                 #{
                                   \new Lyrics \with {
                                     $(if (ly:context-mod? lyric-mods) lyric-mods #{ \with {} #})
                                     $(let ((lyric-mods (assoc-get (glue-symbol `(lyric-mods ,v) "-") options #f #f)))
                                        (if (ly:context-mod? lyric-mods) lyric-mods #{ \with {} #}))
                                     \consists \editionEngraver $piece
                                   } \lyricsto $vocname { \getMusic #`(lyrics ,v) }
                                 #}) verses))
              #{
                \new Lyrics \with {
                  $(if (ly:context-mod? lyric-mods) lyric-mods #{ \with {} #})
                  \consists \editionEngraver $piece
                } \lyricsto $vocname { \getMusic lyrics }
              #})
         %}
       >>
     #}))

\registerTemplate lalily.vocal.lyrics
#(define-music-function (parser location piece options)(list? list?)
   (let ((v (ly:assoc-get 'verse options '() #f))
         (rr (ly:assoc-get 'repeats options #f #f))
         (lyric-mods (assoc-get 'lyric-mods options #f #f))
         (voc (ly:assoc-get 'lyric-voice options "sop" #f)))
     (ly:message "-> ~A" options)
     (if (list? rr)
         (make-music 'SimultaneousMusic 'elements
           (map (lambda (r)
                  #{
                    \keepWithTag $r \new Lyrics \with {
                      $(if (ly:context-mod? lyric-mods) lyric-mods #{ \with {} #})
                      $(let ((lyric-mods (assoc-get (glue-symbol `(lyric-mods ,v) "-") options #f #f)))
                         (if (ly:context-mod? lyric-mods) lyric-mods #{ \with {} #}))
                      \consists \editionEngraver $piece
                    } \lyricsto $voc { \getMusic #v }
                  #}) rr))
         #{
           \new Lyrics \with {
             $(if (ly:context-mod? lyric-mods) lyric-mods #{ \with {} #})
             $(let ((lyric-mods (assoc-get (glue-symbol `(lyric-mods ,@v) "-") options #f #f)))
                (if (ly:context-mod? lyric-mods) lyric-mods #{ \with {} #}))
             \consists \editionEngraver $piece
           } \lyricsto $voc { \getMusic #v }
         #}
         )))


\clratree lalily_vocal_group_default
\addatree lalily_vocal_group_default sop.staff-mods \with { instrumentName = "Sopran" }
\addatree lalily_vocal_group_default alt.staff-mods \with { instrumentName = "Alt" }
\addatree lalily_vocal_group_default ten.staff-mods \with { instrumentName = "Tenor" }
\addatree lalily_vocal_group_default ten.clef "G_8"
\addatree lalily_vocal_group_default bas.staff-mods \with { instrumentName = "Bass" }
\addatree lalily_vocal_group_default bas.clef "bass"
\registerTemplate lalily.vocal.group
#(let ((choir 0))
   (define (get-choir) (set! choir (+ choir 1)) (format "choir~A" choir))
   (define-music-function (parser location piece options)(list? list?)
     (let ((groupmod (ly:assoc-get 'groupmod options #f #f))
           (prefix (ly:assoc-get 'prefix options (get-choir) #f))
           (staffs (ly:assoc-get 'staffs options lalily_vocal_group_default #f))
           (staff-mods (ly:assoc-get 'staff-mods options #f #f))
           (mensur (ly:assoc-get 'mensur options #f))
           (verses (ly:assoc-get 'verses options #f))
           (repeats (ly:assoc-get 'repeats options #f)))
       #{
         \new StaffGroup \with {
           $(if (ly:context-mod? groupmod) groupmod)
           \consists \editionEngraver $piece
           \override BarLine.allow-span-bar = $(if mensur #t #f )
           \override BarLine.transparent = $(if mensur #t #f )
         } $(make-music 'SimultaneousMusic 'elements
              (map (lambda (staff)
                     (let* ((key (assoc-get 'music (cdr staff) (list (car staff))))
                            (vocname (string-append
                                      (assoc-get 'prefix (cdr staff) prefix)
                                      (assoc-get 'vocname (cdr staff) (glue-list key "-"))
                                      ))
                            (opts (assoc-set-all!
                                   (get-default-options (create-music-path #f key) location)
                                   `((vocname . ,vocname)(verses . ,verses)(repeats . ,repeats),@(cdr staff))
                                   ))
                            (instr (ly:assoc-get 'instrument opts #f #f))
                            (templ (cond
                                    ((symbol? instr) `(.. ,instr))
                                    ((list? instr) `(.. ,@instr))
                                    (else '(..))
                                    ))
                            (staff-mods-loc (ly:assoc-get 'staff-mods opts #f #f))
                            )
                       (cond
                        ((and
                          (ly:context-mod? staff-mods)
                          (ly:context-mod? staff-mods-loc))
                         (set! opts (assoc-set! opts 'staff-mods #{ \with { #staff-mods #staff-mods-loc } #})))
                        ((ly:context-mod? staff-mods) (set! opts (assoc-set! opts 'staff-mods staff-mods)))
                        )
                       #{ \callTemplate #templ #key #opts #}
                       )) staffs))
       #})))

