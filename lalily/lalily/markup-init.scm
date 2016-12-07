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

(use-modules (lalily markup)(lalily laly))

(re-export register-markup-producer)
(re-export get-markup-producer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; on-the-fly commands

(define (is-one-page layout props)
        (define (ancestor layout)
                "Return the topmost layout ancestor"
                (let ((parent (ly:output-def-parent layout)))
                     (if (not (ly:output-def? parent))
                         layout
                         (ancestor parent))))
        (and (chain-assoc-get 'page:is-bookpart-last-page props #f)
             (chain-assoc-get 'page:is-last-bookpart props #f)
             (= (chain-assoc-get 'page:page-number props -1)
                (ly:output-def-lookup (ancestor layout) 'first-page-number))))
(define-public (one-page layout props arg)
        (if (is-one-page layout props)
            (interpret-markup layout props arg)
            empty-stencil))
(define-public (not-one-page layout props arg)
        (if (not (is-one-page layout props))
            (interpret-markup layout props arg)
            empty-stencil))

(define-public (running-page layout props arg)
        (let ((pn (chain-assoc-get 'page:page-number props -1))
              (pc (ly:output-def-lookup layout 'pagecopyright (chain-assoc-get 'header:pagecopyright props -2))))
             (if (and (> pn 0) (not (= pn pc)))
                 (interpret-markup layout props arg)
                 empty-stencil)))
(define-public (run-page+last-footer layout props arg)
  (let ((pn (chain-assoc-get 'page:page-number props -1))
        (pc (ly:output-def-lookup layout 'pagecopyright (chain-assoc-get 'header:pagecopyright props -2)))
        (lpmup (ly:output-def-lookup layout 'lastFooterMarkup #f)))
       (if (and (markup? lpmup) (chain-assoc-get 'page:is-bookpart-last-page props #f)
                (chain-assoc-get 'page:is-last-bookpart props #f))
           (interpret-markup layout props lpmup)
           (if (and (> pn 0) (not (= pn pc)) )
               (interpret-markup layout props arg)
               empty-stencil))))

(define-public (first-score-page layout props arg)
        (if (= (chain-assoc-get 'page:page-number props -1) (ly:output-def-lookup layout 'first-page-number))
            (interpret-markup layout props arg)
            empty-stencil))
(define-public (page-copyright layout props arg)
        (let ((pn (chain-assoc-get 'page:page-number props -1))
              (pc (ly:output-def-lookup layout 'pagecopyright (chain-assoc-get 'header:pagecopyright props -2))))
             (if (= pn pc)
                 (interpret-markup layout props arg)
                 empty-stencil)))


(define-public (diff-composer layout props arg)
        (let ((composername (chain-assoc-get 'header:composername props ""))
              (bookname (chain-assoc-get 'header:bookname props "")))
             (if (not (string? composername))(set! composername (markup->string composername #f layout)))
             (if (not (string? bookname))(set! bookname (markup->string bookname #f layout)))
             (if (or
                   (string=? composername bookname)
                   (string=? composername "")
                 )
                 empty-stencil
                 (interpret-markup layout props arg))))

(define-public (has-footnotes layout props arg)
  (if (chain-assoc-get 'header:footnotes props #f)
    (interpret-markup layout props arg)
            empty-stencil))

(define (if-symbol layout props sym arg)
        (let ((val (chain-assoc-get sym props "")))
             (if (or (and (string? val)(string=? val ""))(and (not (string? val)) (null? val)))
                 empty-stencil
                 (interpret-markup layout props arg))))

(define-public (has-piece layout props arg)
        (if-symbol layout props 'header:piece arg))
(define-public (has-bookname layout props arg)
        (if-symbol layout props 'header:bookname arg))
(define-public (has-booktitle layout props arg)
        (if-symbol layout props 'header:booktitle arg))
(define-public (has-publisher layout props arg)
        (if-symbol layout props 'header:publisher arg))
(define-public (has-catname layout props arg)
        (if-symbol layout props 'header:catname arg))
(define-public (has-catnumber layout props arg)
        (if-symbol layout props 'header:catnumber arg))
(define-public (has-ismn layout props arg)
        (if-symbol layout props 'header:ismn arg))
(define-public (has-copyright layout props arg)
        (if-symbol layout props 'header:copyright arg))
(define-public (has-rightinfo layout props arg)
        (if-symbol layout props 'header:rightinfo arg))



(define-public setStyle (define-music-function (parser location sym mup)(symbol? markup?)
    (setstyle sym mup)(make-music 'SequentialMusic 'void #t)))

; redefinition of markup->string function
(set! markup->string la:markup->string)

; plain text markup using markup->string
(define-markup-command (plain-text layout props arg)(markup?)
  (interpret-markup layout props (markup (markup->string arg #f layout props))))

;;; delayed commands using lily-defined 'toc-items'

(define-markup-command (delayed layout props mup)(markup?)
  (let* ((line-part (chain-assoc-get 'line-part props 1))
         (line-width (* (ly:output-def-lookup layout 'line-width) line-part))
         (gauge-stencil (interpret-markup layout props mup)) ; (markup #:super "*" "ABCDEFGHIJKLMNOPQRSTUVWXYZÄÖÜß" )))
         (x-ext (ly:stencil-extent gauge-stencil X))
         (y-ext (ly:stencil-extent gauge-stencil Y)))
        (ly:make-stencil
          `(delay-stencil-evaluation
            ,(delay (ly:stencil-expr
                     
                     (let* ((table (ly:output-def-lookup layout 'label-page-table))
                            (cur-page 0)
                            (current-toc (markup)))
                           (set! cur-page (chain-assoc-get 'page:page-number props -1))
                           (for-each (lambda (toc)
                                             (let ((label (car toc))
                                                   (text (cdr toc)))
                                                  (let ((label-page (and (list? table) (assoc label table))))
                                                       (if (and label-page (>= cur-page (cdr label-page)))
                                                           (set! current-toc (markup->string text #f layout props))))
                                             )
                                     ) (toc-items))
                           (interpret-markup layout (cons (list (cons 'toc:current current-toc)) props) mup)
                     )
          )))
          x-ext y-ext)))

(define-markup-command (fromproperties layout props arg)(list?)
  (let ((mup (get-markup-producer fromproperties-markup)))
       (interpret-markup layout props (mup layout props (list arg)))))
(register-markup-producer fromproperties-markup (lambda (layout props args)
  (let ((ret (markup)))
       (for-each (lambda (sym)(let ((val (chain-assoc-get sym props)))
                              (if (and (or (string? val)(markup? val)) 
                                  (> (string-length (markup->string val)) 0))
                                (set! ret val))))
                 (reverse (car args)))
       ret)))

(define-markup-command (box-ne layout props arg)(markup?)
  (let ((mup (get-markup-producer box-ne-markup)))
       (interpret-markup layout props (mup layout props (list arg)))))
(register-markup-producer box-ne-markup (lambda (layout props args)
          (let ((stil (interpret-markup layout props (car args)))
                (extent-size (lambda (ex) (- (cdr ex)(car ex)))))
               (if (or (> (extent-size (ly:stencil-extent stil X)) 0)
                       (> (extent-size (ly:stencil-extent stil Y)) 0)) (make-rounded-box-markup (car args)) (car args)))))

(define-markup-command (epspage layout props file)(string?)
  (let* ((prop-line-width (chain-assoc-get 'line-width props #f))
         (line-width (or prop-line-width (ly:output-def-lookup layout 'line-width))))
        (eps-file->stencil X line-width file)))

(define-markup-command (combine-list layout props list)(markup-list?)
   (let ((ml (interpret-markup-list layout props list))
         (stil empty-stencil))
     (for-each (lambda (s) (set! stil (ly:stencil-add stil s))) ml)
     stil
   ))

(define-public (paper-height layout props)
  (let ((mm (ly:output-def-lookup layout 'mm))
        (height (ly:output-def-lookup layout 'paper-height))
        (tm (ly:output-def-lookup layout 'top-margin))
        (bm (ly:output-def-lookup layout 'bottom-margin))
        (hom (ly:output-def-lookup layout 'oddHeaderMarkup))
        (hem (ly:output-def-lookup layout 'evenHeaderMarkup))
        (bom (ly:output-def-lookup layout 'oddFooterMarkup))
        (bem (ly:output-def-lookup layout 'evenFooterMarkup)))
       (define (mupheight mup)(if (markup? mup) (let ((y-ext (ly:stencil-extent (interpret-markup layout props mup) Y)))
                        (- (cdr y-ext)(car y-ext))) 0))
       (- height (+ tm bm (max (mupheight hom)(mupheight hem)) 
                          (max (mupheight hom)(mupheight hem)(mupheight markupCopyright))))
))

(define-markup-command (epspageY layout props file)(string?)
  (let* ((perc (chain-assoc-get 'percent props 99))
         (height (* (paper-height layout props) (/ perc 100))))
        (eps-file->stencil Y height file)))


(define-markup-command (hrule layout props)()
  (let* ((th (* (ly:output-def-lookup layout 'line-thickness)
               (chain-assoc-get 'line-thickness props 1)))
        (line-width (chain-assoc-get 'line-width props (ly:output-def-lookup layout 'line-width)))
        (stil (make-line-stencil th 0 0 line-width 0)))
       stil))

(re-export ismn->ean)
(re-export put-stencil)
(re-export get-stencil)


(re-export execMarkup-markup)
(re-export stretch-markup)
(re-export extline-markup)
(re-export sloppyline-markup)
(re-export hilite-markup)
(re-export tquote-markup)
(re-export strftime-markup)
(re-export year-markup)
(re-export copyright-markup)
(re-export style-markup)
(lalily-markup 'style)

(re-export with-props-markup)

(re-export qr-code-markup)
(re-export barcode-markup)

(re-export cache-markup)
(re-export widthalign-markup)
(re-export epsalignX-markup)
(re-export epsalignY-markup)

(define-markup-command (scaled-cache layout props sym scl)(symbol? number-pair?)
  (let ((stil (get-stencil sym)))
       (if (ly:stencil? stil)
           (ly:stencil-scale stil (car scl)(cdr scl))
           (begin (ly:message "unknown stencil '~A'" sym)
                  empty-stencil))
))

(define (book-first-page layout props)
   (define (ancestor layout)
     "Return the topmost layout ancestor"
     (let ((parent (ly:output-def-parent layout)))
       (if (not (ly:output-def? parent))
           layout
           (ancestor parent))))
      (ly:output-def-lookup (ancestor layout) 'first-page-number))

(define-markup-command (with-link layout props label arg)
  (symbol? markup?)
  (let* ((arg-stencil (interpret-markup layout props arg))
         (x-ext (ly:stencil-extent arg-stencil X))
         (y-ext (ly:stencil-extent arg-stencil Y)))
    (ly:make-stencil
     `(delay-stencil-evaluation
       ,(delay (ly:stencil-expr
                (let* ((table (ly:output-def-lookup layout 'label-page-table))
                       (first-page-number (book-first-page layout props))
                       (orig-page-number (if (list? table)
                                        (assoc-get label table)
                                        #f))
                       (p-nr (ly:output-def-lookup layout 'first-page-number))
                       (page-number (+ (if (number? orig-page-number) orig-page-number 1)
                                      (+ 1 (* -1 (if (number? first-page-number) first-page-number 1)))))
                       (link-expr (list 'page-link page-number
                                        `(quote ,x-ext) `(quote ,y-ext))))

                  (ly:stencil-add (ly:make-stencil link-expr x-ext y-ext)
      arg-stencil)))))
           x-ext
           y-ext)))

