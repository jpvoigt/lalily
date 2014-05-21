\version "2.18.0"

#(define lalily-relincl-tmp (ly:get-option 'relative-includes))
#(ly:set-option 'relative-includes #t)
\include "../lalily.ly"
#(ly:set-option 'relative-includes lalily-relincl-tmp)

\parserDefine Path
#(define-scheme-function (parser location p)(list?) p)
\parserDefine PathS
#(define-scheme-function (parser location s p)((char? #\/) string?)
   (map (lambda (e) (string->symbol e)) (string-split p s)))

\parserDefine Pair
#(define-scheme-function (parser location p)(list?)
   (cond
    ((>= (length p) 2)
     (if (> (length p) 2)
     (ly:input-warning location "more than 2 elements: ~A" p))
     (cons (car p) (cadr p)))
    ((> (length p) 0) (cons (car p) #f))
    (else '(#f . #f))
    ))
\parserDefine PairS
#(define-scheme-function (parser location s p)((char? #\|) string?)
   (ly:music-function-exec Pair parser location
     (string-split p s)))

% mirror another music-folder
% needs option 'mirror-path
% may set other options fo the inherited templated
\registerTemplate lalily.mirror
#(define-music-function (parser location piece options)(list? list?)
   (let ((path (assoc-get 'mirror-path options #f #f)))
     (if (not (list? path))
         (begin
          (ly:input-warning location "no mirror-path! (~A | ~A)" path piece)
          (set! path '(..))
          ))
     #{
       \createScoreWithOptions #path #options
     #}))

% create a group
\registerTemplate lalily.group
#(define-music-function (parser location piece options)(list? list?)
   (let* ((elms (assoc-get 'element options '()))
          (group (assoc-get 'group options #f))
          (group-mods (assoc-get 'group-mods options #f))
          (parts (if (> (length elms) 0)
                     (make-music 'SimultaneousMusic 'elements
                       (map
                        (lambda (p)
                          (let* ((opts (cdr p))
                                 (template (assoc-get 'template opts '(generic)))
                                 (path (assoc-get 'music opts (list (car p))))
                                 )
                            #{ \callTemplate ##t #template #path #opts #}
                            )) elms))
                     (make-music 'SimultaneousMusic 'void #t))))
     (if (symbol? group)
         #{
           \new $group \with {
             $(if (ly:context-mod? group-mods) group-mods)
           } $parts
         #}
         parts
         )))

