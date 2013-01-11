\version "2.15.36"
\include "lalily.ly"

\parserDefine applyRhythm
#(let ((seqpred? (lambda (m)
                   (and (ly:music? m)
                        (eq? 'SequentialMusic (ly:music-property m 'name))
                        (list? (ly:music-property m 'elements))))))
   (define-music-function (parser location pat mus)(seqpred? seqpred?)
     (let ((pl '()))
       (for-each
        (lambda (m) (cond
                     ((eq? 'EventChord (ly:music-property m 'name))
                      (set! pl `(,@pl
                                  ,(map (lambda (n) (if (ly:pitch? (ly:music-property n 'pitch))
                                                        (ly:music-property n 'pitch) n))
                                     (ly:music-property m 'elements) ))))
                     ((eq? 'NoteEvent (ly:music-property m 'name))
                      (set! pl `(,@pl (,(ly:music-property m 'pitch)
                                        ,@(ly:music-property m 'articulations)))))
                     )) (ly:music-property mus 'elements))
       (make-music 'SequentialMusic 'elements (map
                                               (lambda (m)
                                                 (let ((ret m))
                                                   (if (eq? 'NoteEvent (ly:music-property m 'name))
                                                       (set! ret
                                                             (make-music 'EventChord
                                                               'elements
                                                               (let ((nl (car pl)))
                                                                 (set! pl (cdr pl))
                                                                 `(,@(map (lambda (p)
                                                                            (if (ly:pitch? p)
                                                                                (make-music 'NoteEvent 'duration (ly:music-property m 'duration) 'pitch p)
                                                                                p)
                                                                            )
                                                                       nl)
                                                                    ,@(ly:music-property m 'articulations)))
                                                               )))
                                                   ret)
                                                 ) (ly:music-property pat 'elements)))
       )))

