;;;; This file is part of lalily, an extension to lilypond <http://www.lilypond.org/>.
;;;;
;;;; Copyright (C) 2011--2012 Jan-Peter Voigt <jp.voigt@gmx.de>
;;;;
;;;; lalily is free software: you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation, either version 3 of the License, or
;;;; (at your option) any later version.
;;;;
;;;; lalily is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with lalily.  If not, see <http://www.gnu.org/licenses/>.

(use-modules (lalily store)(lalily definitions))

(re-export LY_NOOP)

(define-public (log-music-folder)
  (ly:message "music folder: ~A~A"
    (glue-list (get-music-folder) "/")
    (let ((title (get-music-folder-header-field 'title)))
      (if (markup? title) (string-append " \"" (markup->string title) "\"") ""))))
(define-public logMusicFolder
  (define-void-function ()()
    (log-music-folder)))

(define-public (write-lalily-log-file . options)
  (let ((logfile (format "~A~A.log" (ly:parser-output-name (*parser*)) (ly:assoc-get 'suffix options ".lalily" #f))))
    (if (not (equal? logfile
                     (get-registry-val '(lalily runtime logfile-written))))
        (ly:message "writing '~A' ..." logfile))
    (set-registry-val '(lalily runtime logfile-written) logfile)
    (with-output-to-file logfile
      (lambda ()
        (let ((incl (ly:assoc-get 'includes options #t #f))
              (reg (ly:assoc-get 'registry options #t #f))
              (templ (ly:assoc-get 'templates options #t #f))
              (templ-ref (ly:assoc-get 'template-ref options #t #f))
              (pers (ly:assoc-get 'persons options #f #f))
              (mus (ly:assoc-get 'music options #t #f))
              (def (ly:assoc-get 'defaults options #t #f))
              (quot (ly:assoc-get 'quotes options #t #f))
              (edit (ly:assoc-get 'edition-mods options #t #f)))
          (format #t "lalily V ~A @ ~A" (glue-list lalily:version ".") (strftime "%d.%m.%Y %H:%M:%S" lalily:init))(newline)
          (if incl (begin
                    (display "Includes: ---")(newline)
                    (for-each (lambda (p) (format #t "'~A'" p)(newline))
                      (get-registry-val '(lalily runtime loaded)))
                    (newline)
                    ))
          (if reg (begin
                   (display "Registry: ---")(newline)
                   (display-registry)
                   (newline)
                   ))
          (if templ (begin
                     (display "Templates: ---")(newline)
                     (display-templates)
                     (newline)
                     ))
          (if templ-ref (begin
                         (display "Template reference: ---")(newline)
                         (display-template-ref)
                         (newline)
                         ))
          (if pers (begin
                    (display "Personen: ---")(newline)
                    (display-person-store)
                    (newline)
                    ))
          (if mus (begin
                   (display "Musik: ---")(newline)
                   (display-music-pieces)
                   (newline)
                   ))
          (if def (begin
                   (display "Musik Default: ---")(newline)
                   (display-default-music)
                   (newline)
                   ))
          (if quot (begin
                    (display "Musik Quotes: ---")(newline)
                    (display-quotes)
                    (newline)
                    ))
          (if edit (begin
                    (display "Edition Modifications: ---")(newline)
                    (display-mods)
                    (newline)
                    ))
          )))
    ))

(re-export put-music)
(re-export get-music)
(re-export has-music)
(re-export load-music)
(re-export get-music-deep)
(re-export collect-music)
(re-export display-music-pieces)

(define-public registerMusicLoadCallback
  (define-void-function (proc)(procedure?)
    (let ((cbs (get-registry-val lalily:get-music-load-callbacks '())))
      (set-registry-val lalily:get-music-load-callbacks (cons proc cbs))
      )))
(define-public registerMusicStoreCallback
  (define-void-function (proc)(procedure?)
    (let ((cbs (get-registry-val lalily:get-music-store-callbacks '())))
      (set-registry-val lalily:get-music-store-callbacks (cons proc cbs))
      )))

(define-public aGetMusic
  (define-music-function (path)(list?)
    (get-music)))
(define-public aGetMusicDeep
  (define-music-function (path defm skey)(list? (ly:music? #f) scheme?)
    (get-music-deep path skey defm)))
(define-public aPutMusic
  (define-music-function (path music)(list? ly:music?)
    (put-music path music)
    (make-music 'SimultaneousMusic 'void #t)))
(define-public aDelMusic
  (define-music-function (path)(list?)
    (put-music path #f)
    (make-music 'SimultaneousMusic 'void #t)))
(define-public aScratchMusic
  (define-music-function (path music)(list? ly:music?)
    (put-music path music)
    music))
(define-public aSkipMusic
  (define-music-function (path)(list?)
    (let* ((music (get-music path))
           (m (ly:music-length music)))
      (make-music 'SkipEvent
        'duration (ly:make-duration 0 0 (ly:moment-main-numerator m)(ly:moment-main-denominator m ))))))
(define-public aRestMusic
  (define-music-function (path)(list?)
    (let ((music (get-music music)))
      (mmrest-of-length music)
      )))

(re-export register-template)
(re-export get-template)
(re-export call-template)
(re-export display-templates)

(define-public registerTemplate
  (define-void-function (name fun)(list? ly:music-function?)
    (register-template name fun)))

(re-export create-template-path)
(re-export create-music-path)
(re-export musicPath)
(re-export templatePath)

(define-public callTemplate
  (define-music-function
   (tabs name mabs music options)
   ((boolean? #f) list? (boolean? #f) list? list?)
   (call-template (create-template-path tabs name) (create-music-path mabs music) options)))
(define-public stackTemplate
  (define-music-function
   (tabs name mabs piece options sym vals)
   ((boolean? #f) list? (boolean? #f) list? list? string-or-symbol? list?)
   (let ((tmpl (create-template-path tabs name))
         (music (create-music-path mabs piece)))
     (if (string? sym) (set! sym (string->symbol sym)))
     (make-music 'SimultaneousMusic
       'elements
       (map (lambda (x)
              (call-template tmpl music (assoc-set! options sym x))
              ) vals)))))
(define-public loopTemplate
  (define-music-function
   (kind tabs name mabs piece options sym vals)
   (symbol? (boolean? #f) list? (boolean? #f) list? list? string-or-symbol? list?)
   (let ((tmpl (create-template-path tabs name))
         (music (create-music-path mabs name)))
     (if (string? sym) (set! sym (string->symbol sym)))
     (make-music kind
       'elements
       (map (lambda (x)
              (call-template tmpl music (assoc-set! options sym x))
              ) vals)))))

(re-export get-current-music)
(re-export display-music-stack)
(re-export get-current-template)
(re-export display-template-stack)
(re-export get-music-folder)
(re-export set-music-folder!)
(re-export set-default-template)
(re-export get-default-template)
(re-export get-default-options)
(re-export get-default-options-cumul)
(re-export display-default-music)

(define-public getCurrentMusic
  (define-scheme-function ()()
    (let ((ret (get-current-music)))
      (if ret ret (get-music-folder)))))

(define-public setDefaultTemplate
  (define-music-function (piece template options)(list? list? list?)
    (begin
     (set-default-template piece template options)
     (make-music 'SequentialMusic 'void #t))))
(define-public getTemplate
  (define-scheme-function ()()
    (get-default-template (get-music-folder))))
(define-public setTemplate
  (define-music-function (tmpl)(list?)
    (let* ((piece (get-music-folder))
           (opts (get-default-options piece))
           )
      (set-default-template piece tmpl opts)
      )
    (make-music 'SequentialMusic 'void #t)))
(define-public getOptions
  (define-scheme-function (path)(list?)
    (get-default-options (create-music-path #f path))))
(define-public setOptions
  (define-void-function (path opts)(list? list?)
    (let* ((piece (create-music-path #f path))
           (cmf (get-music-folder))
           (tmpl (get-default-template piece))
           )
      (set-default-template piece tmpl opts)
      (set-music-folder! cmf)
      )))
(define-public addOptions
  (define-void-function (path opts)(list? list?)
    (let* ((piece (create-music-path #f path))
           (dopts (get-default-options piece))
           (cmf (get-music-folder))
           (tmpl (get-default-template piece))
           )
      (set-default-template piece tmpl (assoc-set-all! dopts opts))
      (set-music-folder! cmf)
      )))

(define-public optionsInit clratree)
(define-public optionsGet getatree)
(define-public optionsSet setatree)
(define-public optionsAdd addatree)
(define-public optionsRemove rematree)
(define-public optionsAddAll setatreeall)

(define-public getOption
  (define-scheme-function (path field default)((list? '()) string-or-symbol? (scheme? #f))
    (let* ((piece (create-music-path #f path))
           (opts (get-default-options piece))
           )
      (if (string? field) (set! field (string->symbol field)))
      (assoc-get field opts default)
      )))
(define-public setOption
  (define-music-function (piece field val)((list? '()) string-or-symbol? scheme?)
    (let ((cmf (get-music-folder)))
      (if (string? field) (set! field (string->symbol field)))
      (set-default-option parser (create-music-path #f piece) field val)
      (set-music-folder! cmf)
      (make-music 'SequentialMusic 'void #t)
      )))
(define-public removeOption
  (define-music-function (piece field)((list? '()) string-or-symbol?)
    (let ((cmf (get-music-folder)))
      (if (string? field) (set! field (string->symbol field)))
      (remove-default-option parser (create-music-path #f piece) field)
      (set-music-folder! cmf)
      (make-music 'SequentialMusic 'void #t)
      )))

(define-public aCreateScore
  (define-music-function (music)(list?)
    (call-template (get-default-template music)
      music (get-default-options music))))
(define-public createScore
  (define-music-function (music)(list?)
    (let ((piece (create-music-path #f music)))
      (call-template (get-default-template piece)
        piece (get-default-options piece)))))
(define-public createScoreWithOptions
  (define-music-function (music options)(list? list?)
    (let ((piece (create-music-path #f music)))
      (call-template (get-default-template piece)
        piece (assoc-set-all! (get-default-options piece) options)))))

(define-public getMusicFolder
  (define-scheme-function ()()
    (let ((ret (get-music-folder)))
      (if ret ret (get-music-folder)))))
(define-public setMusicFolder
  (define-music-function (path)(list?)
    (set-music-folder! (create-music-path #t path))
    (make-music 'SequentialMusic 'void #t)))

(define-public changeMusicFolder
  (define-music-function (path)(list?)
    (set-music-folder! (create-music-path #f path))
    (make-music 'SequentialMusic 'void #t)))

(define-public musicFolderPath
  (define-scheme-function (path)(list?)
    (create-music-path #f path)))


(re-export set-default-header)
(re-export get-default-header)
(re-export get-music-folder-header-field)

(define-public aSetDefaultHeader
  (define-music-function (piece field value)(list? string-or-symbol? markup?)
    (begin
     (if (string? field) (set! field (string->symbol field)))
     (set-default-header piece field value)
     (make-music 'SequentialMusic 'void #t)
     )))
(define (music-folder-header-set! field value)
  (begin
   (set-default-header (get-music-folder) field value)
   (make-music 'SequentialMusic 'void #t)
   ))
(define (music-folder-header-remove! field)
  (begin
   (remove-default-header (get-music-folder) field)
   (make-music 'SequentialMusic 'void #t)
   ))

(define-public setHeader
  (define-void-function (piece field value)((list? '()) string-or-symbol? scheme?)
    (let ((cmf (get-music-folder)))
      (if (string? field) (set! field (string->symbol field)))
      (set-default-header create-music-path #f piece) field value)
    (set-music-folder! cmf)))
(define-public removeHeader
  (define-void-function (piece field)((list? '()) string-or-symbol?)
    (let ((cmf (get-music-folder)))
      (if (string? field) (set! field (string->symbol field)))
      (remove-default-header (create-music-path #f piece) field)
      )))

(define-public getHeader
  (define-scheme-function (path field default)((list? '()) string-or-symbol? scheme?)
    (let ((p (create-music-path #f path)))
      (if (string? field) (set! field (string->symbol field)))
      (get-default-header p field default))))
(define-public inheritHeader
  (define-void-function (path field)((list? '(..)) symbol?)
    (let ((p (create-music-path #f path)))
      (if (string? field) (set! field (string->symbol field)))
      (music-folder-header-set! field (get-default-header p field))
      )))
(define-public inheritHeaders
  (define-void-function (path fields)((list? '(..)) list?)
    (let ((p (create-music-path #f path)))
      (for-each
       (lambda (field)
         (if (string? field) (set! field (string->symbol field)))
         (music-folder-header-set! field (get-default-header p field))
         ) fields)
      )))
(define-public inheritAllHeaders
  (define-void-function (path)(list?)
    (let* ((p (create-music-path #f path))
           (head (assoc-get 'header (get-default-options p) '() #f)))
      (for-each
       (lambda (p)
         (music-folder-header-set! (car p) (cdr p))
         ) head)
      )))

(define-public setTocLabel
  (define-music-function (value)(markup?)
    (music-folder-header-set! 'toc-label value)))
(define-public setDedication
  (define-music-function (value)(markup?)
    (music-folder-header-set! 'dedication value)))
(define-public setSection
  (define-music-function (value)(markup?)
    (music-folder-header-set! 'section value)))
(define-public setTitle
  (define-music-function (value)(markup?)
    (music-folder-header-set! 'title value)))
(define-public setSubTitle
  (define-music-function (value)(markup?)
    (music-folder-header-set! 'subtitle value)))
(define-public setSubSubTitle
  (define-music-function (value)(markup?)
    (music-folder-header-set! 'subsubtitle value)))
(define-public setPiece
  (define-music-function (value)(markup?)
    (music-folder-header-set! 'piece value)))
(define-public setOpus
  (define-music-function (value)(markup?)
    (music-folder-header-set! 'opus value)))
(define-public setInstrument
  (define-music-function (value)(markup?)
    (music-folder-header-set! 'instrument value)))


(define-public (get-music-folder-options)
  (get-default-options (get-music-folder)))

(define-public (get-default-paper piece)
  (let* ((opts (get-default-options piece))
         (paper (ly:assoc-get 'paper opts #f #f)))
    (if (ly:output-def? paper) paper #{ \paper { } #})))
(define-public (get-music-folder-paper)
  (get-default-paper (get-music-folder)))
(define-public setPaper
  (define-void-function (paper)(ly:output-def?)
    (let* ((piece (get-music-folder))
           (tmpl (get-default-template piece))
           (opts (get-default-options piece)))
      (set-default-template piece tmpl (assoc-set! opts 'paper paper))
      )))

(define-public (get-default-layout piece)
  (let* ((opts (get-default-options piece))
         (layout (ly:assoc-get 'layout opts #f #f)))
    (if (ly:output-def? layout) layout #{ \layout { } #})))
(define-public (get-music-folder-layout)
  (get-default-layout (get-music-folder)))
(define-public setLayout
  (define-void-function (layout)(ly:output-def?)
    (let* ((piece (get-music-folder))
           (tmpl (get-default-template piece))
           (opts (get-default-options piece)))
      (set-default-template piece tmpl (assoc-set! opts 'layout layout))
      )))

(define-public (get-default-midi piece)
  (let* ((opts (get-default-options piece))
         (midi (ly:assoc-get 'midi opts #f #f)))
    (if (ly:output-def? midi) midi #{ \midi { } #})))
(define-public (get-music-folder-midi)
  (get-default-midi (get-music-folder)))
(define-public setMidi
  (define-void-function (midi)(ly:output-def?)
    (let* ((piece (get-music-folder))
           (tmpl (get-default-template piece))
           (opts (get-default-options piece)))
      (set-default-template piece tmpl (assoc-set! opts 'midi midi))
      )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; music functions

; create a template
(define-macro (make-template code)
  `(define-music-function
    (piece options)
    (list? list?)
    ,code))

; get relative
(define-public getMusic
  (define-music-function (defm path)((ly:music? #f) list?)
    (let ((p (create-music-path #f path)))
      (if defm
          (if (has-music? p) (get-music p) defm)
          (get-music p)))))
(define-public getMusicIf
  (define-music-function (proc path)
    ((procedure? (lambda (p) (has-music p (ly:make-moment 0 0)))) list?)
    (let ((p (create-music-path #f path)))
      (if (proc p) (get-music p) (make-music 'SimultaneousMusic 'void #t)))))
(define-public getMusicDeep
  (define-music-function (defm skey)((ly:music? #f) scheme?)
    (let ((p (create-music-path #f '())))
      (get-music-deep p skey defm))))
(define-public collectMusic
  (define-music-function (simul path sym)((boolean? #f) list? scheme?)
    (let ((pal (cond ((procedure? sym) sym)
                 (else (lambda (p) (append p (list sym))))))
          (pred (lambda (p m) #t)))
      (make-music (if simul 'SimultaneousMusic 'SequentialMusic)
        'elements (collect-music (create-music-path #f path) pal pred))
      )))
(define-public putMusic
  (define-music-function (path music)((list? '()) ly:music?)
    (let ((p (create-music-path #f path)))
      (put-music p music)
      (make-music 'SimultaneousMusic 'void #t))))
(define-public scratchMusic
  (define-music-function (path music)(list? ly:music?)
    (let ((p (create-music-path #f path)))
      (put-music p music)
      music)))
(define (music-or-list? mol)(or (ly:music? mol)(list? mol)))
(define-public skipMusic
  (define-music-function (music)(music-or-list?)
    (if (list? music)(set! music (get-music (create-music-path #f music))))
    (let ((m (ly:music-length music)))
      (make-music 'SkipEvent
        'duration (ly:make-duration 0 0 (ly:moment-main-numerator m)(ly:moment-main-denominator m))))
    ))
(define-public restMusic
  (define-music-function (music)(music-or-list?)
    (if (list? music)(set! music (get-music (create-music-path #f music))))
    (mmrest-of-length music)
    ))

; quotes/cues
(define-public createQuote
  (define-void-function (path)(list?)
    (let* ((p (create-music-path #f path)))
      (track-quote p (*location*))
      )))
(define-public aCreateQuote
  (define-void-function (path)(list?)
    (let* ((p (create-music-path #t path)))
      (track-quote p (*location*))
      )))

(define-public aCueMusic
  (define-music-function (path dir mus)(list? integer? ly:music?)
    (let* ((p (create-music-path #t path)))
      (track-quote p (*location*))
      #{
        \cueDuring $(quote-name p) $dir $mus
      #})))
(define-public cueMusic
  (let ((staffnr 0)
        (cuenr 0))
    (define (cue-id) (set! cuenr (+ 1 cuenr)) (format "cue~A" cuenr))
    (define (alignlyrics direction)(if (eq? UP direction) 'alignAboveContext 'alignBelowContext))
    (define-music-function (path opts dir mus)(list? (list? '()) integer? ly:music?)
      (let ((p (create-music-path #f path))
            (cuename (ly:assoc-get 'cuename opts #f #f))
            (instrname (ly:assoc-get 'instrname opts #f #f))
            (clef (ly:assoc-get 'clef opts #f #f))
            (init (ly:assoc-get 'voice-init opts #f #f))
            (transp (ly:assoc-get 'transpose opts (ly:make-pitch 0 0 0) #f))
            (resetVoice (ly:assoc-get 'resetVoice opts #{ \oneVoice #} #f))
            (staffid #f)
            (lyrics (ly:assoc-get 'lyrics opts #f #f))
            (cueid (cue-id))
            )
        (define (strmup? v)
          (or (and (string? v)(not (string-null? v)))
              (and (not (string? v))(markup? v))))
        ; dummy engraver to get the parental staff-id
        (define (getstaffid context)
          (let ((staff (ly:context-find context 'Staff)))
            (if (ly:context? staff) (set! staffid (ly:context-id staff)))
            ; this engraver does nothing
            (list)))
        ; engraver to set the lyric alignment
        (define (aligncue context)
          `((initialize .
              ,(lambda (trans)
                 ; if we have a staff-id ...
                 (if (string? staffid)
                     ; ... set the alignment property
                     (ly:context-set-property! context (alignlyrics dir) staffid)
                     ))
              )))
        ;(ly:message "cuename: ~A ~A" cuename (strmup? cuename))
        (track-quote p (*location*))
        #{
          <<
            \tag #'cued \new CueVoice = $cueid \with {
              \consists \editionEngraver ##f
              % get parent staffs context-id
              \consists #getstaffid
              %\consists "Instrument_switch_engraver"
            } {
              $(if (eq? dir UP) #{ \voiceOne #} #{ \voiceTwo #})
              $(if (and (not (ly:music? lyrics))(strmup? cuename)) #{
                \once \override InstrumentSwitch #'direction = #(if (eq? dir UP) UP DOWN)
                \once \override InstrumentSwitch #'X-offset = #-5
                \set instrumentCueName = #(markup #:concat ("(" cuename ")"))
                   #} #{ \unset instrumentCueName #})
              $(if (string? clef) #{ \cueClef $clef #})
              $(if (ly:music? init) init)
              \transpose c' $transp \quoteDuring $(quote-name p) $(skip-of-length mus)
              $(if (string? clef) #{ \cueClefUnset #})
              \unset instrumentCueName
              $(if (strmup? instrname) #{
                \once \override Voice.InstrumentSwitch #'stencil = ##f
                \set Staff.instrumentCueName = #instrname #})
            }
            {
              \tag #'cued $(if (eq? dir UP) #{ \voiceTwo #} #{ \voiceOne #})
              $mus
            }
            $(if (ly:music? lyrics) #{
              \tag #'cued \new Lyrics \with {
                \consists #aligncue
                \consists \editionEngraver \musicPath #'(cue)
                fontSize = #-2
                \override LyricText #'font-shape = #'italic
                \override StanzaNumber #'font-shape = #'italic
                \override StanzaNumber #'font-series = #'plain
              } \lyricsto $cueid \lyricmode {
                $(if (strmup? cuename) #{
                  \set stanza = \markup { \concat { "(" $cuename ")" } }
                     #}) $lyrics
              }
                 #})
          >>
          \tag #'cued $resetVoice
        #}
        ))))

(define-public aQuoteMusic
  (define-music-function (path mus)(list? ly:music?)
    (let* ((p (create-music-path #t path)))
      (track-quote p (*location*))
      #{
        \quoteDuring $(quote-name p) $mus
      #})))
(define-public quoteMusic
  (define-music-function (path mus)(list? ly:music?)
    (let* ((p (create-music-path #f path)))
      (track-quote p (*location*))
      #{
        \quoteDuring $(quote-name p) $mus
      #})))

(define-public addQuotes
  (define-scheme-function ()()
    (add-tracked-quotes)))


(re-export registerPaper)
(re-export registerLayout)
(re-export registerMidi)
(re-export get-paper)
(define-public getPaper (define-scheme-function (name)(list?)(get-paper name)))
(re-export get-layout)
(define-public getLayout (define-scheme-function (name)(list?)(get-layout name)))
(re-export get-midi)
(define-public getMidi (define-scheme-function (name)(list?)(get-midi name)))


(re-export registerPageTemplate)
(re-export get-page-template)
(re-export call-page-template)
(define-public callPageTemplate
  (define-scheme-function (name options)(list? list?)
    (call-page-template name options)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; markup functions

(define-markup-command (music-folder-header-field layout props field)(symbol?)
  (let* ((text (get-music-folder-header-field field)))
    (if text
        (interpret-markup layout props (markup text))
        empty-stencil)
    ))

