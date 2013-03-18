\version "2.16.0"

#(define lalily-relincl-tmp (ly:get-option 'relative-includes))
#(ly:set-option 'relative-includes #t)
\include "../../lalily.ly"
#(ly:set-option 'relative-includes lalily-relincl-tmp)

\parserDefine getStaffID
#(define-scheme-function (parser location var)(string-or-symbol?)
   (if (string? var)(set! var (string->symbol var)))
   (lambda (context)
     (let ((stc (ly:context-find context 'Staff)))
       (if (ly:context? stc)
           (ly:parser-define! parser var (ly:context-id stc))
           (ly:input-warning location "no Staff found!")
           ))
     '())
   )

\parserDefine alignContext
#(define-scheme-function (parser location dir var)(integer? string-or-symbol?)
   (if (string? var)(set! var (string->symbol var)))
   (lambda (context)
     (let ((stid (ly:parser-lookup parser var)))
       (if (string-null? stid)
           (begin
            (ly:input-warning location "no context ID!")
            '())
           `((initialize . ,(lambda (trans)
                              (ly:context-set-property! context
                                (if (< dir 0) 'alignBelowContext 'alignAboveContext) stid))))
           ))))



