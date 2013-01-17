\include "lalily.ly"

\parserDefine emptyPage
#(define-scheme-function (parser location)()
  #{
    \bookpart { \paper { $(get-paper '(lalily empty-head-foot)) } \markup \null }
#})
