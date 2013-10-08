%%%% This file is part of lalily, an extension to lilypond <http://www.lilypond.org/>.
%%%%
%%%% Copyright (C) 2011--2012 Jan-Peter Voigt <jp.voigt@gmx.de>
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

\version "2.16.0"

#(define lalily-relincl-tmp (ly:get-option 'relative-includes))
#(ly:set-option 'relative-includes #t)
\include "../lalily.ly"
#(ly:set-option 'relative-includes lalily-relincl-tmp)

#(define (format-cat n)
   (cond ((list? n)(glue-list (map (lambda (x) (format-cat x)) n) "."))
     ((integer? n)(format "~2,'0d" n))
     (else (format "~A" n))
     ))
#(define-markup-command (cat-number layout props)()
   (let ((catnr (format-cat (chain-assoc-get 'header:catnumber props 0))))
     (interpret-markup layout props (markup catnr))))
#(define-markup-command (cat-format layout props val)(scheme?)
   (let ((catnr (format-cat val)))
     (interpret-markup layout props (markup catnr))))

#(define-public (jpv:print-version layout)(ly:output-def-lookup layout 'print-version #f))
#(define-markup-command (jpv-cond-override layout props callback sym arg)(procedure? symbol? markup?)
   (interpret-markup layout props (markup #:override (cons sym (callback layout props)) arg)))
#(define-public versionString (string-append "LP-" (lilypond-version) "|" (strftime "%d.%m.%Y-%H:%M" (localtime (current-time)))))
#(define-markup-command (versionMarkup layout props)()
   (if (jpv:print-version layout)
       (interpret-markup layout props (markup #:tiny #:sans versionString))
       empty-stencil))

markupCopyright = \markup
\execMarkup #(lambda (layout props)
               (chain-assoc-get 'header:markupCopyright props
                 (ly:output-def-lookup layout 'markupCopyright
                   #{
                     \markup {
                       \column {
                         \fill-line {
                           \jpv-cond-override #(lambda (layout props)(if (jpv:print-version layout) 0 2.5)) #'baseline-skip
                           \left-column {
                             \line { \copyright \on-the-fly #has-rightinfo { ", " \fromproperty #'header:rightinfo } }
                             {
                               \teeny \sans \line {
                                 "Vervielfältigungen jeglicher Art sind gesetzlich verboten / Any unauthorized reproduction is prohibited by law"
                             } }
                             \versionMarkup
                           }
                           \jpv-cond-override #(lambda (layout props)(if (jpv:print-version layout) 0 2.5)) #'baseline-skip
                           \typewriter \right-column {
                             \on-the-fly #has-catname \concat { \fromproperty #'header:catname "-" \cat-number }
                             \teeny \on-the-fly #has-ismn \concat { "ISMN " \fromproperty #'header:ismn }
                           }
                         }
                       }
                     }
                   #})))
markupCopyrightBack = \markup
\execMarkup #(lambda (layout props)
               (chain-assoc-get 'header:markupCopyright props
                 (ly:output-def-lookup layout 'markupCopyright
                   #{
                     \markup {
                       \column {
                         \fill-line {
                           \jpv-cond-override #(lambda (layout props)(if (jpv:print-version layout) 0 2.5)) #'baseline-skip
                           \typewriter \left-column {
                             \on-the-fly #has-catname \concat { \fromproperty #'header:catname "-" \cat-number }
                             \teeny \on-the-fly #has-ismn \concat { "ISMN " \fromproperty #'header:ismn }
                           }
                           \jpv-cond-override #(lambda (layout props)(if (jpv:print-version layout) 0 2.5)) #'baseline-skip
                           \right-column {
                             \line { \on-the-fly #has-rightinfo { \fromproperty #'header:rightinfo ", " } \copyright }
                             {
                               \teeny \sans \line {
                                 "Vervielfältigungen jeglicher Art sind gesetzlich verboten / Any unauthorized reproduction is prohibited by law"
                             } }
                             \versionMarkup
                           }
                         }
                       }
                     }
                   #})))

\registerPaper #'(lalily default) \paper {
  two-sided = ##t
  inner-margin = 30\mm
  outer-margin = 20\mm
  top-margin = 12\mm
  bottom-margin = 12\mm

  indent = 3

  markup-system-spacing = #'((basic-distance . 12)
                             (minimum-distance . 6)
                             (padding . 1)
                             (stretchability . 5))
  score-markup-spacing = #'((basic-distance . 12)
                            (minimum-distance . 6)
                            (padding . 3)
                            (stretchability . 10))
  score-system-spacing = #'((basic-distance . 12)
                            (minimum-distance . 6)
                            (padding . 4)
                            (stretchability . 15))
  system-system-spacing = #'((basic-distance . 12)
                             (minimum-distance . 6)
                             (padding . 2)
                             (stretchability . 10))
  markup-markup-spacing = #'((basic-distance . 12)
                             (minimum-distance . 6)
                             (padding . 1)
                             (stretchability . 3))
  last-bottom-spacing = #'((basic-distance . 12)
                           (minimum-distance . 6)
                           (padding . 2)
                           (stretchability . 5))
  top-system-spacing = #'((basic-distance . 3)
                          (minimum-distance . 3)
                          (padding . 0)
                          (stretchability . 10))
  top-markup-spacing = #'((basic-distance . 3)
                          (minimum-distance . 0)
                          (padding . 0)
                          (stretchability . 10))


  ragged-last-bottom = ##f
  ragged-bottom = ##f

  ragged-last = ##f
  ragged-right = ##f

  bookTitleMarkup = \markup {
    \override #'(baseline-skip . 3.5)
    \column {
      \fill-line { \fromproperty #'header:dedication }
      \override #'(baseline-skip . 3.5)
      \column {
        \huge \larger \bold
        \fill-line {
          \larger \fromproperty #'header:title
        }
        \fill-line {
          \large \smaller \bold
          \larger \fromproperty #'header:subtitle
        }
        \fill-line {
          \smaller \bold
          \fromproperty #'header:subsubtitle
        }
        \fill-line {
          \fromproperty #'header:poet
          % { \large \bold \fromproperty #'header:instrument }
          \fromproperty #'header:composer
        }
        \fill-line {
          \fromproperty #'header:meter
          \fromproperty #'header:arranger
        }
      }
    }
  }

  oddHeaderMarkup = \markup \fill-line { \null \box-ne \fromproperty #'header:instrument }
  evenHeaderMarkup = \markup \null
  titleFooterMarkup = \markup \bold \execMarkup #(lambda (layout props)
                                                   (let* ((msection (chain-assoc-get 'header:section props))
                                                          (ssection (if (markup? msection) (string-trim-both (markup->string msection)) ""))
                                                          (mtitle (chain-assoc-get 'header:title props))
                                                          (stitle (if (markup? mtitle) (string-trim-both (markup->string mtitle)) ""))
                                                          (mtoc (chain-assoc-get 'toc:current props))
                                                          (stoc (if (markup? mtoc) (string-trim-both (markup->string mtoc)) ""))
                                                          (mlist (list)))
                                                     (if (> (string-length ssection) 0) (set! mlist (list (markup #:italic msection))))
                                                     (if (> (string-length stitle) 0) (set! mlist `(,@mlist ,@(if (> (length mlist) 0) (list ", ")(list)) ,mtitle)))
                                                     (if (and (> (string-length stoc) 0) (not (string=? stitle stoc)))
                                                         (set! mlist `(,@mlist ,@(if (> (length mlist) 0) (list ", ")(list)) ,mtoc)))
                                                     (make-line-markup mlist)
                                                     ))

  oddFooterMarkup = \markup \delayed \column {
    \on-the-fly #page-copyright \markupCopyright

    \on-the-fly #run-page+last-footer {
      \override #'(baseline-skip . 2.5)
      \column {
        \sloppyline {
          \fontsize #-3 {
            \concat {
              \concat {
                \on-the-fly #has-bookname
                \italic \concat { \fromproperty #'header:bookname ", " }
                \on-the-fly #has-booktitle
                \italic \concat { \fromproperty #'header:booktitle ", " }
              }
              \on-the-fly #diff-composer \concat { \fromproperties #'(header:composername header:composer) ", " }
              \execMarkup #(lambda (layout props)(ly:output-def-lookup layout 'titleFooterMarkup
                                                   (markup #:bold #:fromproperties '(header:title toc:current)) ))
              \on-the-fly #has-piece \concat { ", " \fromproperty #'header:piece }
            }
          }
          \on-the-fly #has-catname \concat { \hspace #2 \bold \fontsize #-3 { \sans \concat { \fromproperty #'header:catname "-" \cat-number } } }
          \on-the-fly #has-copyright \concat { \hspace #2 \fontsize #-3 { \char #169 " " \year " " \fromproperty #'header:copyright } }

          \on-the-fly #not-one-page \bold { \hspace #1 \fromproperty #'page:page-number-string }
        }
      }
    }
  }
  evenFooterMarkup = \markup \delayed \column {
    \on-the-fly #page-copyright \markupCopyright

    \on-the-fly #run-page+last-footer {
      \override #'(baseline-skip . 1)
      \column {
        \sloppyline {
          \on-the-fly #not-one-page \bold { \fromproperty #'page:page-number-string \hspace #1 }

          \on-the-fly #has-copyright \concat { \fontsize #-3 { \char #169 " " \year " " \fromproperty #'header:copyright } \hspace #2 }
          \on-the-fly #has-catname \concat { \bold \fontsize #-3 { \sans \concat { \fromproperty #'header:catname "-" \cat-number } } \hspace #2 }

          \fontsize #-3 {
            \concat {
              \on-the-fly #diff-composer \concat { \fromproperties #'(header:composername header:composer) ", " }
              \execMarkup #(lambda (layout props)(ly:output-def-lookup layout 'titleFooterMarkup
                                                   (markup #:bold #:fromproperties '(header:title toc:current)) ))
              \on-the-fly #has-piece \concat { ", " \fromproperty #'header:piece }
              \concat {
                \on-the-fly #has-bookname
                \italic \concat { ", " \fromproperty #'header:bookname }
                \on-the-fly #has-booktitle
                \italic \concat { ", " \fromproperty #'header:booktitle }
              }
            }
          }
        }
      }
    }
  }

  tocTitleMarkup = \markup \huge \column {
    %\override #`(line-width . ,tocWidth)
    \fill-line { \huge \bold "Inhalt" }
    \hspace #1
  }
  tocItemMarkup = \markup {
    %\override #`(line-width . ,tocWidth)
    \fill-with-pattern #0.3 #RIGHT . \concat { \hspace #0 \bold \fromproperty #'toc:text } \fromproperty #'toc:page
  }
  tocCollMarkup = \markup {
    %\override #`(line-width . ,tocWidth)
    \fill-line { \concat { \hspace #0 \bold \fromproperty #'toc:text } \vspace #1 }
  }
  tocPartMarkup = \markup {
    %\override #`(line-width . ,tocWidth)
    \fill-with-pattern #0.3 #RIGHT . \concat { \hspace #2 \fromproperty #'toc:text } \fromproperty #'toc:page
  }
}

\registerPaper #'(lalily empty-head-foot) \paper {
  oddHeaderMarkup = \markup \null
  evenHeaderMarkup = \markup \null
  oddFooterMarkup = \markup \null
  evenFooterMarkup = \markup \null
}
