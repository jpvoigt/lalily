\version "2.18.0"

#(define lalily-relincl-tmp (ly:get-option 'relative-includes))
#(ly:set-option 'relative-includes #t)
\include "../lalily.ly"
#(ly:set-option 'relative-includes lalily-relincl-tmp)

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

\parserDefine inheritHeader
#(define-void-function (parser location path field)((list? '(..)) symbol?)
   (let ((p (create-music-path #f path)))
     (music-folder-header-set! parser location field (get-default-header p field))
     ))
\parserDefine inheritHeaders
#(define-void-function (parser location path fields)((list? '(..)) list?)
   (map (lambda (field)
          (let ((p (create-music-path #f path)))
            (music-folder-header-set! parser location field (get-default-header p field))
            )) fields))
