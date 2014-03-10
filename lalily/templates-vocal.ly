\version "2.17.29"

#(define lalily-relincl-tmp (ly:get-option 'relative-includes))
#(ly:set-option 'relative-includes #t)
\include "../lalily.ly"
#(ly:set-option 'relative-includes lalily-relincl-tmp)

\registerTemplate lalily.vocal
#(define-music-function (parser location piece options)(list? list?)
   (let ((clef (assoc-get 'clef options "G" #f))
         (vocname (assoc-get 'vocname options #f #t))
         (staff-mods (assoc-get 'staff-mods options #f #f))
         (voice-mods (assoc-get 'voice-mods options #f #f))
         (lyric-mods (assoc-get 'lyric-mods options #f #f)))
     (if (not (string? vocname))
         (let ((tmpname (glue-list piece "-")))
           (ly:input-warning location "using ~A as vocname!" tmpname)
           (set! vocname tmpname)
           ))
     #{
       <<
         \new Staff \with {
           $(if (ly:context-mod? staff-mods) staff-mods #{ \with {} #})
           \consists \editionEngraver $piece
         } \new Voice = $vocname \with {
           $(if (ly:context-mod? voice-mods) voice-mods #{ \with {} #})
         } <<
           \getMusicDeep #'meta
           { \callTemplate #'(/ global voice) #'() #'() \clef $clef \getMusic music }
         >>
         \new Lyrics \with {
           $(if (ly:context-mod? lyric-mods) lyric-mods #{ \with {} #})
           \consists \editionEngraver $piece
         } \lyricsto $vocname { \getMusic lyrics }
       >>
     #}))

\clratree lalily_vocal_group_default
\addatree lalily_vocal_group_default sop.staff-mods \with { instrumentName = "Sopran" }
\addatree lalily_vocal_group_default alt.staff-mods \with { instrumentName = "Alt" }
\addatree lalily_vocal_group_default ten.staff-mods \with { instrumentName = "Tenor" }
\addatree lalily_vocal_group_default ten.clef "G_8"
\addatree lalily_vocal_group_default bas.staff-mods \with { instrumentName = "Bass" }
\addatree lalily_vocal_group_default bas.clef "bass"
\registerTemplate lalily.vocal.group
#(define-music-function (parser location piece options)(list? list?)
   (let ((groupmod (ly:assoc-get 'groupmod options #f #f))
         (staffs (ly:assoc-get 'staffs options lalily_vocal_group_default #f))
         (mensur (ly:assoc-get 'mensur options #f)))
     #{
       \new StaffGroup \with {
         $(if (ly:context-mod? groupmod) groupmod)
         \override BarLine.allow-span-bar = $(if mensur #t #f )
         \override BarLine.transparent = $(if mensur #t #f )
       } $(make-music 'SimultaneousMusic 'elements
            (map (lambda (staff)
                   (let* ((key (assoc-get 'music (cdr staff) (list (car staff))))
                          (vocname (string-append
                                    (assoc-get 'prefix (cdr staff) "")
                                    (assoc-get 'vocname (cdr staff) (format "~A" key))
                                    ))
                          (opts (assoc-set-all!
                                 (get-default-options (create-music-path #f key) location)
                                 (cons `(vocname . ,vocname)(cdr staff))
                                 ))
                          (instr (ly:assoc-get 'instrument opts #f #f))
                          (templ (cond
                                  ((symbol? instr) `(.. ,instr))
                                  ((list? instr) `(.. ,@instr))
                                  (else '(..))
                                  )))
                     #{ \callTemplate #templ #key #opts #}
                     )) staffs))
     #}))

