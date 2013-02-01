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

(define-module (lalily markup))

(use-modules (srfi srfi-1)(lily)(lalily lascm)(lalily laly))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; register markup producer

(define-public (register-markup-producer name proc) #f)
(define-public (get-markup-producer name) #f)

(let ((table (list)))
     (set! register-markup-producer (lambda (name proc)
               (set! table (assoc-set! table name proc))))
     (set! get-markup-producer (lambda (name)
               (assoc-get name table))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; some markup commands

; execute markup-lambda(layout props)
(define-markup-command (execMarkup layout props proc)(procedure?)
  (let ((m (proc layout props)))
       (cond ((ly:stencil? m) m)
             ((markup? m)(interpret-markup layout props m))
             (else (interpret-markup layout props (markup (format "~A" m))))
)))
(define-markup-list-command (execMarkupList layout props proc)(procedure?)
  (let ((ml (proc layout props)))
       (map (lambda (m)
                    (cond ((ly:stencil? m) m)
                          ((markup? m)(interpret-markup layout props m))
                          ((string? m)(interpret-markup layout props (markup m)))
                          (else (interpret-markup layout props (markup (format "~A" m))))
            )) ml)
))

(define-markup-command (stretch layout props str mup)(pair? markup?)
  (let* ((stil (interpret-markup layout props mup))
         (len (lambda (p)(- (cdr p) (car p))))
         (x-ext (ly:stencil-extent stil X))
         (y-ext (ly:stencil-extent stil Y))
         (scx (/ (car str) (len x-ext)))
         (scy (/ (cdr str) (len y-ext))))
       (ly:stencil-scale stil scx scy)))


(define (stack-stencils-line-list layout axis dir padding stils)
        "Stack stencils STILS in direction AXIS, DIR, using a list of PADDING."
        (let ((th (ly:output-def-lookup layout 'line-thickness)))
             (cond
               ((null? stils) empty-stencil)
               ((null? (cdr stils)) (car stils))
               (else 
                 (ly:stencil-combine-at-edge
                   (ly:stencil-combine-at-edge
                     (car stils)
                     axis dir
                     (make-line-stencil th 0 0 (car padding) 0) 0)
                   axis dir
                   (stack-stencils-line-list layout axis dir (cdr padding) (cdr stils))
                   0)))))
(define (get-fill-space word-count line-width text-widths)
        (cond
          ((null? text-widths) '())
          
          ;; special case first padding
          ((= (length text-widths) word-count)
           (cons 
             (- (- (/ line-width (1- word-count)) (car text-widths))
                (/ (car (cdr text-widths)) 2))
             (get-fill-space word-count line-width (cdr text-widths))))
          ;; special case last padding
          ((= (length text-widths) 2)
           (list (- (/ line-width (1- word-count))
                    (+ (/ (car text-widths) 2) (car (cdr text-widths)))) 0))
          (else
            (cons 
              (- (/ line-width (1- word-count))
                 (/ (+ (car text-widths) (car (cdr text-widths))) 2))
              (get-fill-space word-count line-width (cdr text-widths))))))
(define-markup-command (extline layout props args)
  (markup-list?)
  (let 
    ((text-direction RIGHT)
     (word-space 1)
     (line-width #f))
    (let* ((orig-stencils (interpret-markup-list layout props args))
           (stencils
             (map (lambda (stc)
                          (if (ly:stencil-empty? stc)
                              point-stencil
                              stc)) orig-stencils))
           (text-widths
             (map (lambda (stc)
                          (if (ly:stencil-empty? stc)
                              0.0
                              (interval-length (ly:stencil-extent stc X))))
                  stencils))
           (text-width (apply + text-widths))
           (word-count (length stencils))
           (prop-line-width (chain-assoc-get 'line-width props #f))
           (line-width (or line-width (ly:output-def-lookup layout 'line-width)))
           (fill-space
             (cond
               ((= word-count 1) 
                (list
                  (/ (- line-width text-width) 2)
                  (/ (- line-width text-width) 2)))
               ((= word-count 2)
                (list
                  (- line-width text-width)))
               (else 
                 (get-fill-space word-count line-width text-widths))))
           (fill-space-normal
             (map (lambda (x)
                          (if (< x word-space)
                              word-space
                              x))
                  fill-space))
           
           (line-stencils (if (= word-count 1)
                 (list
                   point-stencil
                   (car stencils)
                   point-stencil)
                 stencils)))
          
          (if (= text-direction LEFT)
              (set! line-stencils (reverse line-stencils)))
          
          (if (null? (remove ly:stencil-empty? orig-stencils))
              empty-stencil
              (stack-stencils-line-list layout X
                RIGHT fill-space-normal line-stencils)))))

(define-markup-command (sloppyline layout props args)(markup-list?)
  (let* ((stils (remove ly:stencil-empty? (interpret-markup-list layout props args)))
         (stils (if (null? stils) (list empty-stencil) stils))
         (widths (map (lambda (stil)
                              (- (cdr (ly:stencil-extent stil X))
                                 (car (ly:stencil-extent stil X)))) stils))
         (stil-width (apply + widths))
         (line-width (chain-assoc-get 'line-width props #f))
         (line-width (or line-width (ly:output-def-lookup layout 'line-width)))
         (pad (/ (- line-width stil-width) (if (> (length widths) 1) (- (length widths) 1) 2)))
         (th (ly:output-def-lookup layout 'line-thickness))
        )
        (if (< pad 0)
            (let ((xs (/ line-width stil-width)))
              (set! stils (map (lambda (stil) (ly:stencil-scale stil xs 1)) stils))
              (set! pad 0)
              ))
        (if (chain-assoc-get 'draw-line props #f #f)
          (if (> (length stils) 1)
              
              (let ((ret (car stils))
                    (stils (cdr stils)))
                   (for-each (lambda (stil)
                                     (set! ret (ly:stencil-combine-at-edge ret X 1                                    
                                                 (ly:stencil-combine-at-edge 
                                                   (make-line-stencil th 0 0 pad 0) 
                                                   X 1 stil 0) 0)))
                             stils)
                   ret
              )
              
              (ly:stencil-combine-at-edge 
                (make-line-stencil th 0 0 pad 0) X 1
                (ly:stencil-combine-at-edge (car stils) X 1 (make-line-stencil th 0 0 (/ pad 2) 0) 0)
                0)
              
          )
          (stack-stencils X 1 pad stils)
        )
))

(define-markup-command (hilite layout props mup)(markup?)
    "hilite markup"
    (let* ((gob (chain-assoc-get 'edge-round props 0.1))
           (gauge-stencil (interpret-markup layout props mup))
           (x-ext (extent-size (ly:stencil-extent gauge-stencil X) gob))
           (y-ext (extent-size (ly:stencil-extent gauge-stencil Y) gob))
           (color (chain-assoc-get 'hilite-color props yellow)))
          (interpret-markup layout props
            (markup #:combine #:with-color color #:filled-box x-ext y-ext gob mup))))

(define-markup-command (tquote layout props arg)(markup?)
  (let ((mup (get-markup-producer tquote-markup)))
       (interpret-markup layout props (mup layout props (list arg)))))
(register-markup-producer tquote-markup (lambda (layout props args)
          (let ((mup (car args)))
                   (markup #:concat ("„" mup "“")))))

(define-markup-command (strftime layout props arg)(string?)
  (let ((mup (get-markup-producer strftime-markup)))
       (interpret-markup layout props (mup layout props (list arg)))))
(register-markup-producer strftime-markup (lambda (layout props args)
  (let ((format (car args)))
    (if (string? format)
        (markup (strftime format (localtime (current-time))))
	(markup #:with-color red "NO TIME FORMAT"))
)))

(define-markup-command (year layout props)()
  (let ((mup (get-markup-producer year-markup)))
       (interpret-markup layout props (mup layout props (list)))))
(register-markup-producer year-markup (lambda (layout props args)
  (markup (strftime "%Y" (localtime (current-time))))))

(define-markup-command (copyright layout props)()
  (let ((mup (get-markup-producer copyright-markup)))
       (interpret-markup layout props (mup layout props (list)))))
(register-markup-producer copyright-markup (lambda (layout props args)
  (markup #:char 169 #:year #:fromproperty 'header:copyright )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; style templates
(define-public (setstyle symbol markup) #f)
(define-public (getstyle symbol) #f)
(let ((style:markups (list)))
     (set! setstyle (lambda (symbol markup)
                   (set! style:markups (assoc-set! style:markups symbol markup))))
     (set! getstyle (lambda (symbol)
               (let ((m #f))
                    (set! m (assoc-get symbol style:markups))
                    (if m m (markup #:italic #:fromproperty 'style:text)))))
)
(define-markup-command (style layout props symbol markup)(symbol? markup?)
  (interpret-markup layout (cons (list (cons 'style:text markup)) props) (getstyle symbol)))

(register-markup-producer style-markup (lambda (layout props args)
          (let* ((sym (car args))
                 (arg (cdr args)))
                (markup #:override `(style:text . ,arg) (getstyle sym)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; a markup-command-independent markup->string function

; taken from markup.scm
(define (markup-function? x)
        (and (markup-command-signature x)
             (not (object-property x 'markup-list-command))))

(define-public (la:markup->string mup . mprops)
  (let ((conc (if (> (length mprops) 0) (car mprops) #f))
        (layout (if (> (length mprops) 1) (cadr mprops) #f))
        (props (if (> (length mprops) 2) (caddr mprops) '()))
        (mup? (lambda (m)(or (string? m)(list? m)(markup? m))))
        (result ""))
       (cond ((string? mup) (set! result mup))
             ((null? mup) (set! result ""))
             
             ((and (pair? mup) (equal? (car mup) concat-markup)) 
              (set! result (la:markup->string (cdr mup) #t layout props)))
             
             ((and (pair? mup) (equal? (car mup) fromproperty-markup)) 
              (set! result (la:markup->string (chain-assoc-get (cadr mup) props "???") conc layout props)))
             ((and (pair? mup) (equal? (car mup) override-markup)) 
              (set! result (la:markup->string (cdr (cdr mup)) conc layout (cons (list (car (cdr mup))) props))))
             ((and (pair? mup) (equal? (car mup) page-ref-markup))
              (set! result (let* ((table (if layout (ly:output-def-lookup layout 'label-page-table) '()))
                                  (pg (assoc-get (car (cdr mup)) table)))
                                 (if pg (format "~A" pg) (caddr (cdr mup)))) ))
             
             ((and (pair? mup)(markup-function? (car mup)))
              (let ((proc (get-markup-producer (car mup))))
                   (if (procedure? proc)
                     (set! result (la:markup->string (proc layout props (cdr mup))))
                     (for-each (lambda (m)(set! result (string-append result (if (or conc (string=? result "")) "" " ") (la:markup->string m conc layout props))))
                               (filter mup? (cdr mup))))))
             ((list? mup)
              (for-each (lambda (m)(set! result (string-append result (if (or conc (string=? result "")) "" " ") (la:markup->string m conc layout props))))
                        (filter mup? mup)))
             (else (set! result (format "?~A?" mup))(ly:message "markup->string: '~A'" mup)))
       result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; barcodes
(define-markup-command (qr-code layout props str size idn)(string? number? boolean?)
  (let ((tmp (format "~A-~2A.eps" (strftime "%Y%m%d%H%M%S" (localtime (current-time))) (random 100)))
        (qr-stencil (interpret-markup layout props (markup #:with-color red #:filled-box (cons 0 size) (cons 0 size) 0.7))))
       (if (symbol? str)(let ((tmp (chain-assoc-get str props #f)))
                (if tmp (set! str tmp))))
       (system (format (if idn 
                         "echo \"~A\" | idn --quiet | qrencode -o - -m 0 | convert PNG:- BMP:- | potrace -a -1 -o \"~A\""
                         "echo \"~A\" | qrencode -o - -m 0 | convert PNG:- BMP:- | potrace -a -1 -o \"~A\"")
                       str tmp))
       (set! qr-stencil (eps-file->stencil X size tmp))
       (system (format "rm -v \"~A\"" tmp))
       qr-stencil
))
(define-markup-command (barcode layout props type str size nums)(string? string-or-symbol? number-pair? boolean?)
  (let ((width (car size))
        (height (cdr size)))
       (if (symbol? str)(let ((tmp (chain-assoc-get str props #f)))
                (set! str (if tmp tmp (symbol->string str)))))
       (if (string=? "EAN" type)(set! str (ismn->ean str)))
       (let ((tmp (format "~A-~2A.eps" (strftime "%Y%m%d%H%M%S" (localtime (current-time))) (random 100)))
             (barcode-stencil (interpret-markup layout props (markup #:with-color red #:filled-box (cons 0 width) (cons 0 height) 0.7))))
            (if (> (string-length str) 0) (begin
                  (system (format "barcode ~A -e \"~A\" -g ~Ax~A+0+0 -o \"~A\" -E -b \"~A\"" (if nums "" "-n") type (* 100 width) (* 100 height) tmp str))
                  (set! barcode-stencil (eps-file->stencil X (car size) tmp))
                  (system (format "rm -v \"~A\"" tmp))
            ))
            barcode-stencil
)))

(define-public (ismn->ean ismn)
  (let ((ismn-list (string->list ismn)))
       (set! ismn-list (filter (lambda (c) (char-numeric? c)) ismn-list))
       (if (or (= 8 (length ismn-list))(= 9 (length ismn-list)))(set! ismn-list (append (string->list "9790") ismn-list)))
       (cond
         ((= (length ismn-list) 13)(let ((p 0)(i 0))
               (for-each (lambda (c)(let ((n (- (char->integer c)(char->integer #\0))))
                                      (set! i (1+ i))
                                      (if (= 0 (modulo i 2))(set! n (* 3 n)))
                                      (set! p (+ p n))
                         )) ismn-list)
               (if (not (= 0 (modulo p 10))) (begin (ly:message "invalid ismn: checksum ~A" ismn)(set! ismn-list '())))
         ))
         ((= (length ismn-list) 12)(let ((p 0)(i 0))
               (for-each (lambda (c)(let ((n (- (char->integer c)(char->integer #\0))))
                                      (set! i (1+ i))
                                      (if (= 0 (modulo i 2))(set! n (* 3 n)))
                                      (set! p (+ p n))
                         )) ismn-list)
               (set! ismn-list (append ismn-list `( ,(integer->char (+ (char->integer #\0) (- 10 (modulo p 10)) )) )))
         ))
         (else (begin (ly:message "invalid ismn ~A" ismn)(set! ismn-list '()))))
       (list->string ismn-list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; stencil cache
(define-public (put-stencil symbol stencil) #f)
(define-public (get-stencil symbol) #f)
(let ((cache (list)))
     (set! put-stencil (lambda (symbol stencil)
                      (set! cache (assoc-set! cache symbol stencil))))
     (set! get-stencil (lambda (symbol)
                      (assoc-get symbol cache)))
)

(define-markup-command (cache layout props sym text)(symbol? markup?)
  (let ((stencil (get-stencil sym)))
       (if (not stencil)
           (set! stencil (interpret-markup layout props text)))
       (put-stencil sym stencil)
       stencil
))

(define-markup-command (widthalign layout props cache arg)(symbol? markup?)
  (let ((stil (get-stencil cache))
        (perc (chain-assoc-get 'percent props 100)))
       (if stil (let* ((ext (ly:stencil-extent stil X))
                       (width (- (cdr ext) (car ext))))
                      (set! props `( ((line-width . ,(* width (/ perc 100)))) . ,props) ))
                (ly:message "no stencil ~A" cache))
       (interpret-markup layout props arg) ))

(define-markup-command (epsalignX layout props cache file)(symbol? string?)
  (let ((stil (get-stencil cache))
        (perc (chain-assoc-get 'percent props 100)))
       (if stil
         (let* ((ext (ly:stencil-extent stil X))
                (width (- (cdr ext) (car ext))))
               (eps-file->stencil X (* width (/ perc 100)) file))
         (begin (ly:message "no stencil ~A" cache) empty-stencil))))

(define-markup-command (epsalignY layout props cache file)(symbol? string?)
  (let ((stil (get-stencil cache))
        (perc (chain-assoc-get 'percent props 100)))
       (if stil
         (let* ((ext (ly:stencil-extent stil Y))
                (height (- (cdr ext) (car ext))))
               (eps-file->stencil Y (* height (/ perc 100)) file))
         (begin (ly:message "no stencil ~A" cache) empty-stencil))))

