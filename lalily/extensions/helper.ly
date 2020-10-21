\version "2.20.0"

% define context property
defineContextProperty =
#(define-void-function (name pred? description)(symbol? procedure? string?)
   (if (not (memq name all-translation-properties))
       ((@@ (lily) translator-property-description) name pred? description)
       ))
defineContextID =
#(define-void-function (name pred? description)(symbol? procedure? string?)
   (ly:input-warning (*location*) "deprecated!
use \\defineContextProperty ~A ~A \"~A\"" name (procedure-name pred?) description)
   (defineContextProperty name pred? description)
   )

