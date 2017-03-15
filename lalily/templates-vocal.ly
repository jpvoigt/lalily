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

\version "2.19.32"

#(define lalily-relincl-tmp (ly:get-option 'relative-includes))
#(ly:set-option 'relative-includes #t)
\include "../lalily.ly"
#(ly:set-option 'relative-includes lalily-relincl-tmp)

%{
init vocal Voice context. Default: \dynamicUp \autoBeamOff
looks for music named 'init-vocal
%}
\registerTemplate lalily.init.Voice.vocal
#(define-music-function (piece options)(list? list?)
   (let ((deepdef (assoc-get 'deepdef options #{ \dynamicUp \autoBeamOff #}))
         (deepsym (assoc-get 'deepsym options 'init-vocal))
         (init-opts (assoc-get 'init-opts options '())))
     #{
       \callTemplate ##t lalily.init.Voice #'() #(assoc-set-all! init-opts `((deepdef . ,deepdef)(deepsym . ,deepsym)))
     #}))

%{
create one staff with one vocal voice and associated lyrics.
%}
\registerTemplate lalily.vocal
#(define-music-function (piece options)(list? list?)
   (let ((init-opts (assoc-get 'init-opts options '() #f))
         (clef (assoc-get 'clef options #f #f))
         (vocname (assoc-get 'vocname options #f #t))
         (vocname-proc #f)
         (staffname (assoc-get 'staffname options #f #f))
         (staff-context (assoc-get 'staff-context options 'Staff #f))
         (staff-mods (assoc-get 'staff-mods options #f #f))
         (voice-mods (assoc-get 'voice-mods options #f #f))
         ;(voices (assoc-get 'voices options #f #f)) % TODO two voices in staff
         ;(lyric-mods (assoc-get 'lyric-mods options #f #f))
         ;(repeats (assoc-get 'repeats options #f #f))
         (verses (assoc-get 'verses options #f #f))
         (lyrics (assoc-get 'lyrics options '() #f))
         (upper (assoc-get 'upper options #f #f)))
     (define (do-upper) (and (list? upper) (list? (assoc-get 'music upper #f #f))))
     (if (procedure? vocname)
         (begin
          (set! vocname-proc vocname)
          (set! vocname (vocname piece options))
          ))
     (if (not (string? vocname))
         (let ((tmpname (glue-list piece "-")))
           (ly:input-warning (*location*) "using ~A as vocname!" tmpname)
           (set! vocname tmpname)
           ))
     (if (not (string? staffname)) (set! staffname (glue-list piece ":")))
     #{
       <<
         \new $staff-context = $staffname \with {
           $(if (ly:context-mod? staff-mods) staff-mods #{ \with {} #})
           \consists \editionEngraver $piece
         } <<
           \callTemplate voice #'() #(assoc-set-all! options
                                       `((init-music . ,(if (do-upper) #{ \voiceTwo #} #{ \oneVoice #}))
                                         (vocname . ,vocname)
                                         ))

           $(if (do-upper)
                (let ((path `(.. ,@(assoc-get 'music upper '() #f))) ; TODO absolute path
                       (vocname (assoc-get 'vocname upper vocname-proc #f)))
                  (if (procedure? vocname) (set! vocname (vocname (create-music-path #f path) upper)))
                  (if (not (string? vocname))
                      (let ((tmpname (glue-list (create-music-path #f path) "-")))
                        (ly:input-warning (*location*) "using ~A as vocname!" tmpname)
                        (set! vocname tmpname)
                        ))
                  (set! upper (assoc-set-all! options
                                `(,@upper
                                   (init-music . ,#{ \voiceOne #} )
                                   (vocname . ,vocname)
                                   )))
                  #{
                    \callTemplate voice #path #upper
                  #}))

         >>
         $(if (do-upper)
              (let ((vocname (assoc-get 'vocname upper "" #f))
                    (lyrics (assoc-get 'lyrics upper (if (> (length lyrics) 0) #f '())))
                    (lyric-mods (assoc-get 'lyric-mods upper #f #f))
                    (verses (assoc-get 'verses upper verses #f)))
                (set! lyric-mods #{
                  \with {
                    $(if (ly:context-mod? lyric-mods) lyric-mods #{ \with {} #})
                    alignAboveContext = $staffname
                      } #})
                (if (list? lyrics)
                    #{
                      $(if (list? verses)
                           #{ \stackTemplate lyrics #`(.. ,@lyrics) #(assoc-set-all! upper
                                                                       `((lyric-voice . ,vocname)
                                                                         (lyric-mods . ,lyric-mods))
                                                                       ) #'verse #(map (lambda (v) (list v)) verses) #}
                           #{ \callTemplate lyrics #`(.. ,@lyrics) #(assoc-set-all! upper
                                                                      `((lyric-voice . ,vocname)
                                                                        (lyric-mods . ,lyric-mods))
                                                                      ) #})
                    #})))
         $(if (list? verses)
              #{ \stackTemplate lyrics #lyrics #(assoc-set! options 'lyric-voice vocname) #'verse #(map (lambda (v) (list v)) verses) #}
              #{ \callTemplate lyrics #lyrics #(assoc-set! options 'lyric-voice vocname) #})
       >>
     #}))

\registerTemplate lalily.vocal.voice
#(define-music-function (piece options)(list? list?)
   (let ((voice-mods (assoc-get 'voice-mods options #f #f))
         (vocname (assoc-get 'vocname options #f #t))
         (init-opts (assoc-get 'init-opts options '() #f))
         (init-voice (assoc-get 'init-music options (make-music 'SequentialMusic 'void #t) #f))
         (clef (assoc-get 'clef options #f #f))
         (voice-context (assoc-get 'voice-context options 'Voice #f)))
     #{
       \new $voice-context = $vocname \with {
         $(if (ly:context-mod? voice-mods) voice-mods #{ \with {} #})
         $(if (not (eq? 'Voice voice-context)) #{ \with { \consists \editionEngraver #piece } #})
       } <<
         \getMusicDeep meta
         {
           \callTemplate ##t lalily.init.Voice.vocal #'() #init-opts
           $(if (string? clef) #{ \clef $clef #})
           $init-voice $(if (has-music? (get-current-music)) (getMusic '()) (getMusic '(music)))
         }
       >>
     #}))

\registerTemplate lalily.vocal.lyrics
#(define-music-function (piece options)(list? list?)
   (let* ((v (ly:assoc-get 'verse options '() #f))
         (rr (ly:assoc-get 'repeats options #f #f))
         (lyric-mods (assoc-get 'lyric-mods options #f #f))
         (voc (ly:assoc-get 'lyric-voice options "sop" #f))
         (lyric-name (ly:assoc-get 'lyric-name options voc #f)))
     ;(ly:message "-> ~A" options)
     (if (list? rr)
         (make-music 'SimultaneousMusic 'elements
           (map (lambda (r)
                  #{
                    \keepWithTag $r \new Lyrics = $(format "~A-~A" lyric-name r) \with {
                      $(if (ly:context-mod? lyric-mods) lyric-mods #{ \with {} #})
                      $(let ((lyric-mods (assoc-get (glue-symbol `(lyric-mods ,v) "-") options #f #f)))
                         (if (ly:context-mod? lyric-mods) lyric-mods #{ \with {} #}))
                      \consists \editionEngraver $piece
                    } \lyricsto $voc { \getMusic #`(lyrics ,@v) }
                  #}) rr))
         #{
           \new Lyrics = $lyric-name \with {
             $(if (ly:context-mod? lyric-mods) lyric-mods #{ \with {} #})
             $(let ((lyric-mods (assoc-get (glue-symbol `(lyric-mods ,@v) "-") options #f #f)))
                (if (ly:context-mod? lyric-mods) lyric-mods #{ \with {} #}))
             \consists \editionEngraver $piece
           } \lyricsto $voc { \getMusic #`(lyrics ,@v) }
         #}
         )))

\registerTemplate lalily.vocal.two
#(define-music-function (piece opts)(list? list?)
   (let* ((prefix (assoc-get 'prefix opts ""))
          (staff-name (assoc-get 'staffname opts (glue-list piece "-") #f))
          (staff-mods (assoc-get 'staff-mods opts #f #f))
          (lyric-mods (assoc-get 'lyric-mods opts #{ \with {} #}))
          (verses (assoc-get 'verses opts #f #f))
          (repeats (assoc-get 'repeats opts #f #f))
          (lyrics (assoc-get 'lyrics opts '() #f))
          (music-prefix (assoc-get 'music-prefix opts '(..)))
          (upper (assoc-get 'upper opts '()))
          (upper-music (unfold-path (assoc-get 'music upper '()) '()))
          (upper-lyrics (unfold-path (assoc-get 'lyrics upper lyrics) '()))
          (upper-name (string-append prefix (assoc-get 'vocname upper (glue-list upper-music "-"))))
          (upper-init (make-music 'SequentialMusic 'elements `(,(assoc-get 'init upper #{#}) ,#{ \voiceOne #})))
          (upper-lyric-mods #{ \with {
            $lyric-mods
            $(let ((mods (assoc-get 'lyric-mods upper #{ \with {} #}))) mods)
            alignAboveContext = $staff-name
            } #})
          (lower (assoc-get 'lower opts '()))
          (lower-music (unfold-path (assoc-get 'music lower '()) '()))
          (lower-lyrics (unfold-path (assoc-get 'lyrics lower lyrics) '()))
          (lower-name (string-append prefix (assoc-get 'vocname lower (glue-list lower-music "-"))))
          (lower-init (make-music 'SequentialMusic 'elements `(,(assoc-get 'init lower #{#}) ,#{ \voiceTwo #})))
          (lower-lyric-mods #{ \with {
            $lyric-mods
            $(let ((mods (assoc-get 'lyric-mods lower #{ \with {} #}))) mods)
            alignBelowContext = $staff-name
            } #})
          )
     (if (or (= 0 (length upper-music))(not (eq? #\/ (car upper-music))))
         (set! upper-music (append music-prefix upper-music)))
     (if (or (= 0 (length lower-music))(not (eq? #\/ (car lower-music))))
         (set! lower-music (append music-prefix lower-music)))
     (if (or (= 0 (length lyrics))(not (eq? #\/ (car lyrics))))
         (set! lyrics (append music-prefix lyrics)))
     #{
       <<
       \new Staff = $staff-name \with {
         $(if (ly:context-mod? staff-mods) staff-mods #{ \with {} #})
         \consists \editionEngraver $piece
       } <<
         \callTemplate LY_UP.voice #upper-music #(assoc-set-all! opts (append upper `((vocname . ,upper-name)(init-music . ,upper-init))))
         \callTemplate LY_UP.voice #lower-music #(assoc-set-all! opts (append upper `((vocname . ,lower-name)(init-music . ,lower-init))))
       >>
         $(if (list? verses)
              #{
                <<
                  \stackTemplate LY_UP.lyrics #upper-lyrics #(assoc-set-all! upper 
                                                               `((lyric-voice . ,upper-name)
                                                                 (lyric-mods . ,upper-lyric-mods)
                                                                 (repeats . ,repeats)
                                                                 )) #'verse #(map (lambda (v) (list v)) verses)
                  \stackTemplate LY_UP.lyrics #lower-lyrics #(assoc-set-all! lower
                                                               `((lyric-voice . ,lower-name)
                                                                 (lyric-mods . ,lower-lyric-mods)
                                                                 (repeats . ,(if (list? repeats) (reverse repeats) repeats))
                                                                 )) #'verse #(map (lambda (v) (list v)) verses)
              >> #}
              #{
                <<
                \callTemplate LY_UP.lyrics #upper-lyrics #(assoc-set-all! upper
                                                            `((lyric-voice . ,upper-name)
                                                              (lyric-mods . ,upper-lyric-mods)
                                                              (repeats . ,repeats)))
                \callTemplate LY_UP.lyrics #lower-lyrics #(assoc-set-all! lower
                                                            `((lyric-voice . ,lower-name)
                                                              (lyric-mods . ,lower-lyric-mods)
                                                              (repeats . ,(if (list? repeats) (reverse repeats) repeats))
                                                              ))
                >>
              #})
       >>
     #}))


\optionsInit lalily_vocal_group_default
\optionsAdd lalily_vocal_group_default sop.staff-mods \with { instrumentName = "Sopran" }
\optionsAdd lalily_vocal_group_default alt.staff-mods \with { instrumentName = "Alt" }
\optionsAdd lalily_vocal_group_default ten.staff-mods \with { instrumentName = "Tenor" }
\optionsAdd lalily_vocal_group_default ten.clef "G_8"
\optionsAdd lalily_vocal_group_default bas.staff-mods \with { instrumentName = "Bass" }
\optionsAdd lalily_vocal_group_default bas.clef "bass"
\registerTemplate lalily.vocal.group
#(let ((choir 0))
   (define (get-choir) (set! choir (+ choir 1)) (format "choir~A" choir))
   (define-music-function (piece options)(list? list?)
     (let ((groupmod (ly:assoc-get 'group-mods options (ly:assoc-get 'groupmod options #f #f)))
           (prefix (ly:assoc-get 'prefix options (get-choir) #f))
           (staffs (ly:assoc-get 'staffs options lalily_vocal_group_default #f))
           (staff-mods (ly:assoc-get 'staff-mods options #f #f))
           (spanbar (ly:assoc-get 'spanbar options #f))
           (mensur (ly:assoc-get 'mensur options #f))
           (verses (ly:assoc-get 'verses options #f))
           (repeats (ly:assoc-get 'repeats options #f))
           (lyrics (assoc-get 'lyrics options '() #f))
           )
       #{
         \new StaffGroup \with {
           $(if (ly:context-mod? groupmod) groupmod)
           \consists \editionEngraver $piece
           \override BarLine.allow-span-bar = $(if (or spanbar mensur) #t #f )
           \override BarLine.transparent = $(if mensur #t #f )
         } $(make-music 'SimultaneousMusic 'elements
              (map (lambda (staff)
                     (let* ((key (assoc-get 'music (cdr staff) (list (car staff))))
                            (vocname (string-append
                                      (assoc-get 'prefix (cdr staff) prefix)
                                      (assoc-get 'vocname (cdr staff) (glue-list key "-"))
                                      ))
                            (upper (assoc-get 'upper (cdr staff) #f #f))
                            (opts (assoc-set-all!
                                   (get-default-options (create-music-path #f key))
                                   `((vocname . ,vocname)(verses . ,verses)(repeats . ,repeats)(lyrics . ,lyrics),@(cdr staff))
                                   ))
                            (instr (ly:assoc-get 'staff opts #f #f))
                            (templ (cond
                                    ((symbol? instr) `(.. ,instr))
                                    ((list? instr) `(.. ,@instr))
                                    (else '(..))
                                    ))
                            (staff-mods-loc (ly:assoc-get 'staff-mods opts #f #f))
                            )
                       (if (and (list? upper)(list? (assoc-get 'music upper #f #f)))
                           (let ((uvocname (string-append
                                            (assoc-get 'prefix (cdr staff) prefix)
                                            (assoc-get 'vocname upper (glue-list (assoc-get 'music upper) "-"))
                                            )))
                             (assoc-set! opts 'upper (assoc-set! upper 'vocname uvocname))
                             ))

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




%{
convert-ly (GNU LilyPond) 2.19.36  convert-ly: Processing `'...
Applying conversion: 2.19.2, 2.19.7, 2.19.11, 2.19.16, 2.19.22,
2.19.24, 2.19.28, 2.19.29, 2.19.32
%}
