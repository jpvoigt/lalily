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

(define-module (lalily latex))

(use-modules 
  (lily)
  (ice-9 popen)
  (ice-9 rdelim)
  (ice-9 regex)
  (scm framework-eps)
  (lalily lascm)
  (lalily markup))

(define (markup-function? x)
        (and (markup-command-signature x)
             (not (object-property x 'markup-list-command))))

(define-public (content-height layout props)
  (let ((mm (ly:output-def-lookup layout 'mm))
        (height (ly:output-def-lookup layout 'paper-height))
        (perc (chain-assoc-get 'percent props 100))
        (tm (ly:output-def-lookup layout 'top-margin))
        (bm (ly:output-def-lookup layout 'bottom-margin))
        (hom (ly:output-def-lookup layout 'oddHeaderMarkup))
        (hem (ly:output-def-lookup layout 'evenHeaderMarkup))
        (bom (ly:output-def-lookup layout 'oddFooterMarkup))
        (bem (ly:output-def-lookup layout 'evenFooterMarkup)))
       (define (mupheight mup)(if (markup? mup) (let ((y-ext (ly:stencil-extent (interpret-markup layout props mup) Y)))
                        (- (cdr y-ext)(car y-ext))) 0))
       (/ (* (- height (+ tm bm (max (mupheight hom)(mupheight hem)) 
             (max (mupheight hom)(mupheight hem)))) (/ perc 100)) mm)
))

(define-public (markup->tex mup . mprops)
  (let ((conc (if (> (length mprops) 0) (car mprops) #f))
        (layout (if (> (length mprops) 1) (cadr mprops) #f))
        (props (if (> (length mprops) 2) (caddr mprops) '()))
        (mup? (lambda (m)(or (string? m)(list? m)(markup? m))))
        (result ""))
       (cond ((string? mup) (set! result mup))
             ((null? mup) (set! result ""))
             
             ((and (pair? mup) (equal? (car mup) concat-markup)) 
              (set! result (markup->tex (cdr mup) #t layout props)))
             
             ((and (pair? mup) (equal? (car mup) fromproperty-markup)) 
              (set! result (markup->tex (chain-assoc-get (cadr mup) props "???") conc layout props)))
             
             ((and (pair? mup) (equal? (car mup) override-markup)) 
              (set! result (markup->tex (cdr (cdr mup)) conc layout (cons (list (car (cdr mup))) props))))
             ((and (pair? mup) (equal? (car mup) page-ref-markup))
              (set! result (let* ((table (if layout (ly:output-def-lookup layout 'label-page-table) '()))
                                  (pg (assoc-get (car (cdr mup)) table)))
                                 (if pg (format "~A" pg) (caddr (cdr mup)))) ))
             
             ((and (pair? mup)(markup-function? (car mup)))
              (let ((proc (get-markup-producer (car mup))))
                   (if (procedure? proc)
                     (set! result (markup->tex (proc layout props (cdr mup))))
                     (for-each (lambda (m)(set! result (string-append result (if (or conc (string=? result "")) "" " ") (markup->tex m conc layout props))))
                               (filter mup? (cdr mup))))))

             ((list? mup)
              (for-each (lambda (m)(set! result (string-append result (if (or conc (string=? result "")) "" " ") (markup->tex m conc layout props))))
                        (filter mup? mup)))
             (else (ly:message "~A" mup)))
       result))

(define-public (tex-markup-list layout props pkgs cmd opts m)
        (let* ((mm (ly:output-def-lookup layout 'mm))
               (scropts (chain-assoc-get 'scrartcl props ""))
               (size (chain-assoc-get 'line-width props (ly:output-def-lookup layout 'line-width 10)))
               ; width of our box in mm
               (width (let ((tw (chain-assoc-get 'tex-width props #f))) (if tw (begin (set! size (* tw mm)) tw) (/ size mm))))
               ; percent of page to use
               (perc (chain-assoc-get 'percent props))
               ; height of our box in mm
               (height (chain-assoc-get 'tex-height props (- (content-height layout props) (chain-assoc-get 'bottom-gap props (if perc 0 3)))))
               ; the text to fill into template.tex
               (text (markup->tex m #f #f props))
               ; basename of working files
               (basename (strftime (format "~A-%Y%m%d%H%M%S" cmd) (localtime (current-time))))
               ; result of each command
               (result 0)
               ; stencil to return
               (text-stencil empty-stencil)
               (pages 0)
               (epslist '()))
              (define (readpipe port text)
                      (let ((line (read-line port)))
                           (if (not (eof-object? line))
                               (set! text (readpipe port (string-append (string-append text line) "\n"))))
                           text))
              (define (cmd->string cmd)
                      (let* ((port (open-pipe cmd OPEN_READ))
                             (str (readpipe port ""))
                             (result (close-pipe port)))
                            (if (and (= 0 result)(> (string-length str) 0))
                                str "")))
              (define (loop n callback)(let ((ret (list)))
                           (define (lp i)
                                   (if (<= i n)
                                       (begin
                                         (set! ret (append ret (list (callback i))))
                                         (lp (+ i 1)))))
                           (lp 1)
                           ret))
              
              ; write <basename>.tex
              (with-output-to-file (format "~A.tex" basename) (lambda ()
                        (format #t "\\documentclass~A{scrartcl}
\\usepackage[paperwidth=~Amm,paperheight=~Amm,margin=0mm]{geometry}
\\usepackage[~A]{babel}
~A
\\begin{document}
~A
\\end{document}
" 
                                   scropts width height
                                   (chain-assoc-get 'babel props "ngerman") 
                                   (glue-list pkgs "\n")
                                   text)))
              ; produce pdf
              (set! result (system (format "export LD_LIBRARY_PATH=\"\" ; ~A ~A \"~A.tex\"" cmd opts basename)))
              ; how many pages
              (set! pages (let* ((r (make-regexp "Pages:\\s+([0-9]+)"))
                       (m (regexp-exec r (cmd->string (format "pdfinfo \"~A.pdf\"" basename)))))
                      (string->number (match:substring m 1))))
              ; add pages to markup-list
              (loop pages (lambda (pag)(let ((pagname (format "~A-~A.eps" basename pag)))
                             ; convert page to EPS
                             (set! result (system (format "pdftops -eps -f ~A -l ~A \"~A.pdf\" \"~A\"" pag pag basename pagname)))
                             ; include EPS
                             (set! text-stencil (eps-file->stencil X size (format "~A-~A.eps" basename pag)))
                             (set! epslist (append epslist (list text-stencil)))
              )))
              ; remove working files
              (system (format "rm -v \"~A\"*" basename))
              ; return eps-stencil
              epslist
))

