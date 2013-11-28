\version "2.17.29"
%% version 24/04/2012
%% for lilypond 2.16 or higher

%% some utility functions
#(define (name-of music)
 (ly:music-property music 'name))
 
#(define (good-music? music)
   (and (not (eq? 'Music (name-of music)))
        (or (pair? (ly:music-property music 'elements))
            (ly:music? (ly:music-property music 'element))
            (pair? (ly:music-property music 'articulations)))))
 
#(define (noteEvent? music)
(eq? (name-of music) 'NoteEvent))
 
#(define (has-notes? music)
"Return true if there is at least one note in `music, false otherwise."
(let ((found? #f))
  (let loop ((evt music))
    (cond (found?)    ; do nothing : stop loop
          ((noteEvent? evt)(set! found? #t))
          (else       ; search deeper
            (let ((e (ly:music-property evt 'element))
                  (es (ly:music-property evt 'elements)))
              (if (ly:music? e) (loop e))
              (while (and (pair? es)(not found?))
                (begin
                  (loop (car es))
                  (set! es (cdr es))))))))
 found?))
 
#(define (expand-q-chords music); for q chords : see chord-repetition-init.ly
(expand-repeat-chords! (list 'rhythmic-event) music))

#(define-macro (add! lst elt)
"Add element `elt to the list `lst."
`(set! ,lst (append ,lst (list ,elt))))

#(define (first-event-is-note music)
"Explore the first containers for the very first event and return true only if
this event is a note."
(let ((res #f))
  (let loop ((evt music))
    (if (not res)
     (case (name-of evt) 
       ((NoteEvent) (set! res #t))
       ((EventChord) (set! res (has-notes? evt)))  
       (else 
          (let ((e (ly:music-property evt 'element))
                (es (ly:music-property evt 'elements)))
            (if (ly:music? e)(loop e))
            (if (pair? es)(loop (car es)))))))) ; only first elt
  res))

#(define (clean-music mus)
"Try to reduce the number of sequential music"
(let ((name (ly:music-property mus 'name)))
  (cond
    ((eq? name 'SequentialMusic)
       (ly:music-set-property! mus 'elements (fold-right
          (lambda (evt prev-list)
            (if (eq? (ly:music-property evt 'name) 'SequentialMusic)
              (append (ly:music-property (clean-music evt) 'elements) prev-list)
              (cons (clean-music evt) prev-list)))
          '()
          (ly:music-property mus 'elements))))
    ((eq? name 'SimultaneousMusic)
       (ly:music-set-property! mus 'elements
                (map clean-music (ly:music-property mus 'elements))))
    ((memq name (list 'RelativeOctaveMusic 'UnrelativableMusic))
         (ly:music-set-property! mus 'element (clean-music
                  (ly:music-property mus 'element)))))
 mus))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%% changePitch %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#(define cPInsertInPattern (gensym))
#(define cPSamePitch (gensym))
#(define cPSamePitchEnd (gensym))

#(define (make-notes-list music)
"Make a list with each element of one of this type
  1- a note, a chord, a rest, a skip
  2- #f : it will indicate that the next element is of type 3
  3- some music added with \\insert function, inside the `newnotes
     parameter of the \\changePitch function."
(let ((res '()))     ; the list to fill
  (define (fill-notes-list evt)
    (let ((tags (ly:music-property evt 'tags)))
      (cond 
        ((memq cPInsertInPattern tags)        ; a music added by \insert
            (ly:music-set-property! evt 'tags
                          (delq cPInsertInPattern tags)); remove the tag
            (add! res #f)          ; #f will indicate that next elt is
            (add! res evt))        ; an \insert music
         ((memq (name-of evt)(list 
                  'EventChord 'NoteEvent 'RestEvent 'SkipEvent 'SkipMusic))
            (add! res evt))
         (else
          (let ((elt (ly:music-property evt 'element))
                (elts (ly:music-property evt 'elements)))
            (if (ly:music? elt) (fill-notes-list elt))
            (if (pair? elts)(for-each fill-notes-list elts)))))))
(fill-notes-list music)
res))

%%%%%%%%%%%%  used inside change-pitch
#(define-macro (add-insert-music! music-list)
"Check if an element is #f and if so, add the following element to the list"
`(while (and (pair? notes-list)            ; (car notes-list) = #f means next
             (not (car notes-list)))       ;  elt is an \insert music
  (begin (add! ,music-list (cadr notes-list))
         (set! skip-notnote-event? #t)   ; will skip all datas before next note
         (set! notes-list (cddr notes-list)))))
%%%%%%%%%%%%  used inside the inner function change-one-note
#(define (copy-duration from to)  ; from and to as EventChord or NoteEvent
(let ((max-dur #f)); in theory, 2 notes in a chord can have a different duration
  (music-map (lambda (x)            ; get main duration from `from
              (let ((dur (ly:music-property x 'duration)))
               (if (and (ly:duration? dur)
                        (or (not max-dur)
                            (ly:duration<? max-dur dur))); take the greater
                 (set! max-dur dur))
                 x))
              from)
  (music-map (lambda (x)            ; set duration to `to
               (if (ly:duration? (ly:music-property x 'duration))
                  (ly:music-set-property! x 'duration max-dur))
               x)
             to)))

#(define (copy-arti from to) ; from and to as EventChord or NoteEvent
(let* ((es-from (ly:music-property from 'elements))
       (es-to (ly:music-property to 'elements))
       (arti-from (if (null? es-from) 
                    (ly:music-property from 'articulations)
                    (filter 
                      (lambda(x)
                        (not (ly:duration? (ly:music-property x 'duration))))
                      es-from))))
  (if (null? es-to)                       ; NoteEvent
    (ly:music-set-property! to 'articulations  
              (append (ly:music-property to 'articulations) arti-from))
    (ly:music-set-property! to 'elements  ; EventChord
              (append es-to arti-from)))
  ; copy also 'tags and 'to-relative-callback            
  (ly:music-set-property! to 'tags 
    (append (ly:music-property from 'tags)(ly:music-property to 'tags)))
   (if (null? es-to) 
      (ly:music-set-property! to 'to-relative-callback 
          (ly:music-property from 'to-relative-callback))
      (begin
        (ly:music-set-property! to 'to-relative-callback 
            ly:music-sequence::event-chord-relative-callback)
        (ly:music-set-property! (car es-to) 'to-relative-callback
            (ly:music-property from 'to-relative-callback))))
    ))

%% del-arti is called for all notes but the first of a \samePitch section. 
#(define (del-arti note-or-chord)
(ly:music-set-property! note-or-chord 'articulations '())
(ly:music-set-property! note-or-chord 'elements 
  (filter (lambda(x) 
              (and (ly:duration? (ly:music-property x 'duration))
                   (ly:music-set-property! x 'articulations '())))
          (ly:music-property note-or-chord 'elements)));; can be empty
(music-map  ;; del all caution accidentals
  (lambda(x)(if (eq? (name-of x) 'NoteEvent) 
               (ly:music-set-property! x 'force-accidental #f))
             x)
  note-or-chord))

#(define (change-pitch pattern newnotes)
"The scheme function of \\changePitch.`pattern and `newnotes are music.
`pattern must contain at least 1 note."
(let ((notes-list (make-notes-list newnotes)) ; see (make-notes-list)
      (seq-list '())                          ; list of transformed patterns
      (skip-notnote-event? #f) ; set when a skip or \insert is found in newnotes
      (same-pitch-section? #f) ; set for a \samePitch section in pattern
      (must-filter #f))        ; set when filter good-music? has to be called
  (define (change-one-note note res)  ; note = EventChord NoteEvent, res = list
    (cond
      ((null? notes-list)             ; if notes-list is empty
        (set! must-filter #t)         ; returns an undefined music that we
        (make-music 'Music))          ; will have to filter.
      (else (let ((x (car notes-list)))
        (case (name-of x)
            ((or EventChord NoteEvent RestEvent)
                (if same-pitch-section? ; the same x will be used several times
                                   (set! x (del-arti (ly:music-deep-copy x))))
                (copy-duration note x)
                (copy-arti note x)
                (let ((tags (ly:music-property x 'tags)))
                  (cond               ; are we in a \samePitch section ?
                    ((memq cPSamePitch tags)    ; yes, first,remove the tag
                       (ly:music-set-property! x 'tags (delq cPSamePitch tags))
                       (set! same-pitch-section? #t)) ; then set the flag
                    ((memq cPSamePitchEnd tags) ; last note of \samePitch
                       (ly:music-set-property! x 'tags
                                      (delq cPSamePitchEnd tags))
                       (set! same-pitch-section? #f)       ; unset the flag
                       (set! notes-list (cdr notes-list))) ; next new note
                    (else (set! notes-list (cdr notes-list)))))
                 (set! skip-notnote-event? #f); \insert and \skip will delete
                              ; all datas between 2 notes, so stop this here.
                 (add! res x) ; add the modified note                       
                 (add-insert-music! res))  ; is there \insert music ?      
            ((SkipEvent)     ; the new evt was a \skip : nothing will be
                (set! skip-notnote-event? #t)  ; added until a new note is
                (set! notes-list (cdr notes-list))))))); found in pattern
        res)
  (define (change-one-pattern pat)
   (let ((res '()))        ; the container (a list) for the modified evts
    (let loop ((evt pat))
      (cond 
        ((memq (name-of evt)(list 'NoteEvent 'EventChord))
          (set! res (change-one-note evt res)))
        ((or (ly:duration? (ly:music-property evt 'duration))
             (not (has-notes? evt)))
          (if (not (or skip-notnote-event? (null? notes-list))) (add! res evt)))
        (else
          (let ((e (ly:music-property evt 'element))
                (es (ly:music-property evt 'elements))
                (save-res res))
           (if (ly:music? e)(ly:music-set-property! evt 'element (begin
              (set! res (change-one-pattern e))
              (case (length res)
                ((0) (set! must-filter #t)  ; we'll have to
                     res)                   ; filter pat
                ((1) (car res))
                (else (make-sequential-music res))))))
           (if (pair? es)(ly:music-set-property! evt 'elements (begin
              (set! res '())
              (for-each loop es)
              res)))
           (set! res save-res)
           (add! res evt))))
          res)))    ; return the container with the modified list                 
    ;;;;; end of change-one-pattern and beginning of the main function ;;;;;
   (add-insert-music! seq-list)     ; if notes-list begins with an \insert music
   (while (pair? notes-list)
     (let ((evt (car notes-list)))  ; deals first with a special case :
       (if (and evt                                 ; first elt of notes-list
               (eq? 'SkipEvent (name-of evt))       ; is a \skip ? and pattern
               (not (first-event-is-note pattern))) ; is not beginning by a note
          (begin (set! skip-notnote-event? #t)  ; skip all datas before the very
                 (set! notes-list (cdr notes-list)); first note of the pattern.
                 (add-insert-music! seq-list))) ;check then for an \insert music
         ;; now, do the staff : change each notes of pattern with a new pitch ;;
       (let ((new-es (change-one-pattern (ly:music-deep-copy pattern))))
         (if must-filter (set! new-es (filter good-music? new-es)))
         (set! seq-list (append seq-list new-es)) ; add transformed pattern
         (if (> (length notes-list) 2)    ; case here of a \skip then an \insert
            (let ((elt1 (car notes-list))     ; a \skip ?
                  (elt2 (cadr notes-list)))   ; if #f then elt3 = \insert music
               (if (and elt1 (eq? 'SkipEvent (name-of elt1)) (not elt2))
                 (begin  ; the \insert music will be placed between the end of 
                   (set! notes-list (cdr notes-list)) ; the current pattern and 
                   (add-insert-music! seq-list))))))  ; before the next pattern.
         (set! skip-notnote-event? #f)))        ; ready now for the next loop...

   (let ((relativize (lambda(m)
            (let* ((clean-newnotes (clean-music newnotes))
                   (name (name-of clean-newnotes)))
                (if (memq name (list 'RelativeOctaveMusic 'UnrelativableMusic))
                   (make-music name 'element m)
                   m)))))
     (case (length seq-list)
        ((0) (make-music 'Music 'void #t))
        ((1) (relativize (car seq-list)))
        (else (relativize (clean-music (make-sequential-music seq-list))))))))

changePitch = #(define-music-function (parser location pattern newnotes)
                                                          (ly:music? ly:music?)
"Change each notes in `pattern by the notes (or rests) given in `newnotes.
If count of events doesn't match, pattern is duplicated repeatedly or truncate."
(let* ((expand-q (lambda (music) (expand-repeat-chords!
			    (cons 'rhythmic-event (ly:parser-lookup parser '$chord-repeat-events))
			    music)))
       (pattern (expand-q pattern))
       (newnotes (expand-q newnotes)))
(if (has-notes? pattern)               ; avoid endless loops ...
  (change-pitch pattern newnotes)
  pattern)))
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% enhancement functions, working with \changePitch pattern newnotes

samePitch = #(define-music-function (parser location music) (ly:music?)
"Inside the `pattern parameter of the \\changePitch function, you can group
some notes by calling this function. All grouped notes will have the same pitch,
according to the current note of the `newnotes parameter of \\changePitch."
(let((first-note #f)
     (last-note #f))
  (map-some-music
    (lambda (x)
      (case (name-of x)
        ((or NoteEvent EventChord)
           (if (not first-note)
             (begin
               (set! first-note x)
               (ly:music-set-property! x 'to-relative-callback '()))
             (ly:music-set-property! x 'to-relative-callback ; avoid octave pbs
                (lambda (x p)                                ; in \relative mode
                    (ly:prob-set-property! x 'pitch p)
                    p)))
           (ly:music-set-property! x 'tags (cons
                   cPSamePitch  ; add tag cPSamePitch to x
                   (ly:music-property x 'tags)))
           (set! last-note x)   ; save the note x
           x)
        (else #f)))
    music)
  (if last-note              ; the last saved EventChord
     (ly:music-set-property! last-note 'tags (cons
           cPSamePitchEnd    ; add cPSamePitchEnd tag, delete cPSamePitch tag
           (delq cPSamePitch (ly:music-property last-note 'tags)))))
  music))

absolute = #(define-music-function (parser location music) (ly:music?)
"Make `music unrelativable. To use inside a \\samePitch function in relative
mode."
(make-music 'UnrelativableMusic 'element music))

insert = #(define-music-function (parser location music) (ly:music?)
"Using this function inside the `newnotes parameter of the \\changePitch
function, allow you to insert and remplace by `music, all music between one note
and his following, in the `pattern parameter of \\changePitch, ."
#{ \tag #cPInsertInPattern $music #})

%%%%%%%
%{ #(define (n-copy n music)
(if (< n 2)
  music
 (make-sequential-music (map
    (lambda (x)(ly:music-deep-copy music))
    (make-list n))))) %}

#(define (n-copy n music)
(cond
  ((> n 1) (make-sequential-music (map (lambda (x)(ly:music-deep-copy music))
                                       (make-list n))))
  ((= n 1) music)
  (else (make-music 'Music 'void #t))))

nCopy = #(define-music-function (parser location n music)(integer? ly:music?)
(n-copy n music))

%% same effect as { \repeat unfold n s } but \nSkip works inside the `newnotes
%% parameter of \changePitch.
nSkip = #(define-music-function (parser location n)(integer?)
"Return \\skip \\skip \\skip ... n times."
#{
  \nCopy #n s
#})


%{
/usr/bin/python: /home/jpv/lily2.17/lilypond/usr/lib/libz.so.1: no
version information available (required by /usr/bin/python) convert-ly
(GNU LilyPond) 2.17.96  convert-ly: »« wird verarbeitet... Anwenden
der Umwandlung: 2.17.0, 2.17.4, 2.17.5, 2.17.6, 2.17.11, 2.17.14,
2.17.15, 2.17.18, 2.17.19, 2.17.20, 2.17.25, 2.17.27, 2.17.29
%}
