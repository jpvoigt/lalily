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

(define-module (lalily lascm))

(use-modules (srfi srfi-1)(ice-9 regex))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; utilities

(define-public (glue-list lst glue)
  "create string from list containing arbitrary objects"
  (string-join (map (lambda (s)(format "~A" s)) lst) glue 'infix))
(define-public (glue-symbol lst . glue)
  "create symbol from list containig arbitrary objects"
  (string->symbol (string-join (map (lambda (s)(format "~A" s)) lst) (if (> (length glue) 0)(car glue) ":") 'infix)))

(define-public (format-alist l . ind)
  "create string from (a-)list for pretty printing
                                example: (format-alist '((a . 1)(b . 2)))
==>  a=1
     b=2"
(let ((i (if (> (length ind) 1) (cadr ind) 0))
      (istr (if (> (length ind) 0) (car ind) " ")))
  (define (indsp n)(if (> n 0) (string-append istr (indsp (- n 1))) ""))
  (cond
   ((and (pair? l)(markup? (cdr l)))(format "~A~A=~A~&" (indsp (+ i 1)) (car l) (markup->string (cdr l))))
   ((and (list? l)(not (dotted-list? l))(any pair? l))
    (let ((ret ""))
      (for-each (lambda (e)
                  (set! ret (string-append ret (format-alist e istr (+ i 1)))))
        l)
      ret))
   ((pair? l)(let ((k (car l))(v (cdr l)))(format "~A~A=~A~&" (indsp (+ i 1)) k v)))
   (else (format "~A~A~&" (indsp i) l)))
  ))

(define-public (assoc-set-all! lst vls)
  "set all values from vls in lst"
  (begin
   (for-each (lambda (p)
               (set! lst (assoc-set! lst (car p) (cdr p)))) vls)
   lst))

(define-public (rcdr l)(reverse (cdr (reverse l))))
(define-public (rcddr l)(reverse (cddr (reverse l))))
(define-public (rcdddr l)(reverse (cdddr (reverse l))))
(define-public (rcddddr l)(reverse (cddddr (reverse l))))
(define-public (rcar l)(car (reverse l)))
(define-public (rcadr l)(cadr (reverse l)))
(define-public (rcaddr l)(caddr (reverse l)))
(define-public (rcadddr l)(cadddr (reverse l)))

(define-public (with-append-file fn func)
  (let ((fport #f))
    (dynamic-wind
     (lambda ()
       (set! fport (open-file fn "a"))
       (set-current-output-port fport))
     (lambda () (func) (force-output))
     (let ((cp (current-output-port)))
       (lambda ()
         (set-current-output-port cp)
         (if (file-port? fport) (close-port fport))
         (set! fport #f)
         ))
     )))

(define-public (normalize-path path)
  "create list, removing '.. elements
                                example: (normalize-path '(a b .. c d)) ==> '(a c d)"
(let ((ret '()))
  (for-each (lambda (e)
              (set! ret (cond ((eq? e '..)(if (> (length ret) 1) (cdr ret) '()))
                          (else `(,e ,@ret))))) path)
  (reverse ret)))

(define-public (listcwd) '())
(define-public (absolutePath? path) #f)
(let* ((os (getenv "OS"))
       (isWindows (if (and (string? os) (regexp-exec (make-regexp ".*Windows.*" regexp/icase) os)) #t #f))
       (wrx (if isWindows (make-regexp "^[a-z]:$" regexp/icase) #f)))
  (set! listcwd (lambda () (string-split (getcwd)(if isWindows #\\ #\/))))
  (set! absolutePath? (lambda (path) (if isWindows
                                         (if (and (> (length path) 0) (regexp-exec wrx (car path))) #t #f)
                                         (and (> (length path) 0) (= 0 (string-length (car path))))
                                         )))
  )

(define-public (normalize-path-list path)
  "create list, removing \"..\" elements
                                example: (normalize-path '(\"a\" \"b\" \"..\" \"c\" \".\" \"d\")) ==> '(\"a\" \"c\" \"d\")"
(let ((ret '()))
  (for-each (lambda (e)
              (set! ret (cond ((equal? e "..")(if (> (length ret) 1) (cdr ret) (cdr (reverse (listcwd)))))
                          ((equal? e ".") (if (= (length ret) 0) (reverse (listcwd)) ret))
                          (else `(,e ,@ret))))) path)
  (reverse ret)))
(define-public (normalize-path-string s) "create normalized path string: a/b/../c/d ==> a/c/d" (string-join (normalize-path-list (string-split s #\/)) "/" 'infix))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; classes

(use-modules (oop goops)
  (lily))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; stack

(define-class <stack> ()
  (name #:accessor name #:setter set-name! #:init-value "stack")
  (store #:accessor store #:setter set-store! #:init-value '())
  )

(define-method (push (stack <stack>) val)
  (set! (store stack) (cons val (store stack))))
(define-method (get (stack <stack>))
  (let ((st (store stack)))
    (if (> (length st) 0)
        (car st)
        #f)))
(define-method (pop (stack <stack>))
  (let ((st (store stack)))
    (if (> (length st) 0)
        (let ((ret (car st)))
          (set! (store stack) (cdr st))
          ret)
        #f)))
(define-method (display (stack <stack>) port)
  (for-each (lambda (e)
              (format #t "~A> " (name stack))(display e)(newline)) (store stack)))

(define-public (stack-create)(make <stack>))
(export push)
(export get)
(export pop)
(export store)
(export name)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; tree

(define-class <tree> ()
  (children #:accessor children #:init-thunk make-hash-table)
  (key #:accessor key #:init-keyword #:key #:init-value 'node)
  (value #:accessor value #:setter set-value! #:init-value #f)
  )

(define-method (tree-set! (tree <tree>) (path <list>) val)
  (if (= (length path) 0)
      (set! (value tree) val)
      (let* ((ckey (car path))
             (cpath (cdr path))
             (child (hash-ref (children tree) ckey))
             )
        (if (not (is-a? child <tree>))
            (begin (set! child (make <tree> #:key ckey))
              (hash-set! (children tree) ckey child)
              ))
        (tree-set! child cpath val)
        ))
  val)

(define-method (tree-merge! (tree <tree>) (path <list>) (proc <procedure>) val)
  (let ((ctree (tree-get-tree tree path)))
    (if (is-a? ctree <tree>)
        (set! (value ctree) (proc (value ctree) val))
        (tree-set! tree path (proc #f val)))
    ))
(define-method (tree-get-tree (tree <tree>) (path <list>))
  (if (= (length path) 0)
      tree
      (let* ((ckey (car path))
             (cpath (cdr path))
             (child (hash-ref (children tree) ckey))
             )
        (if (is-a? child <tree>)
            (tree-get-tree child cpath)
            #f)
        )))
(define-method (tree-get (tree <tree>) (path <list>))
  (let ((ctree (tree-get-tree tree path)))
    (if (is-a? ctree <tree>) (value ctree) #f)))
(define-method (tree-get-from-path (tree <tree>) (path <list>) skey val)
  (if (equal? skey (key tree))(set! val (value tree)))
  (let ((child (hash-ref (children tree) skey)))
    (if (is-a? child <tree>)(set! val (value child))))
  (if (= (length path) 0)
      val
      (let* ((ckey (car path))
             (cpath (cdr path))
             (child (hash-ref (children tree) ckey))
             )
        (if (is-a? child <tree>)
            (tree-get-from-path child cpath skey val)
            val)
        )))
(define-method (tree-get-keys (tree <tree>) (path <list>))
  (if (= (length path) 0)
      (hash-map->list (lambda (key value) key) (children tree))
      (let* ((ckey (car path))
             (cpath (cdr path))
             (child (hash-ref (children tree) ckey))
             )
        (if (is-a? child <tree>)
            (tree-get-keys child cpath)
            #f)
        )))

(define-method (tree-dispatch (tree <tree>) (path <list>) (relative <list>) def)
  (let ((val (value tree)))
    (if (= (length path) 0)
        (if val (cons '() val)(cons relative def))
        (let* ((ckey (car path))
               (cpath (cdr path))
               (child (hash-ref (children tree) ckey))
               )
          (if (or val (not (list? relative))) (set! relative '()))
          (if val (set! def (value tree)))
          (if (is-a? child <tree>)
              (tree-dispatch child cpath `(,@relative ,ckey) def)
              `((,@relative ,@path) . ,def))
          ))))

(define-method (tree-collect (tree <tree>) (path <list>) (vals <stack>))
  (let ((val (value tree)))
    (if (> (length path) 0)
        (let* ((ckey (car path))
               (cpath (cdr path))
               (child (hash-ref (children tree) ckey))
               )
          (if (is-a? child <tree>) (tree-collect child cpath vals))
          ))
    (if val (push vals val))
    (reverse (store vals))
    ))

(define (stdsort p1 p2)
  (let ((v1 (car p1))
        (v2 (car p2)))
    (cond
     ((and (number? v1) (number? v2)) (< v1 v2))
     ((and (ly:moment? v1) (ly:moment? v2)) (ly:moment<? v1 v2))
     (else (string-ci<? (format "~A" v1) (format "~A" v2)))
     )))
(define-method (tree-walk (tree <tree>) (path <list>) (callback <procedure>) . opts)
  (let ((dosort (assoc-get 'sort opts))
        (sortby (assoc-get 'sortby opts stdsort))
        (doempty (assoc-get 'empty opts)))
    (if (or doempty (value tree))
        (callback path (key tree) (value tree)))
    (for-each (lambda (p)
                (tree-walk (cdr p) `(,@path ,(car p)) callback `(sort . ,dosort) `(sortby . ,sortby) `(empty . ,doempty)))
      (if dosort (sort (hash-table->alist (children tree)) sortby)
          (hash-table->alist (children tree)) ))
    ))
(define-method (tree-walk-branch (tree <tree>) (path <list>) (callback <procedure>) . opts)
  (let ((dosort (assoc-get 'sort opts))
        (sortby (assoc-get 'sortby opts stdsort))
        (doempty (assoc-get 'empty opts))
        (ctree (tree-get-tree tree path)))
    (if (is-a? ctree <tree>)
        (tree-walk ctree path callback `(sort . ,dosort) `(sortby . ,sortby) `(empty . ,doempty)))
    ))
(define-public (tree-display tree . opt)
  (let ((path (ly:assoc-get 'path opt '() #f))
        (dosort (ly:assoc-get 'sort opt #t #f))
        (sortby (assoc-get 'sortby opt stdsort))
        (empty (ly:assoc-get 'empty opt #f #f))
        (dval (ly:assoc-get 'value opt #t #f))
        (vformat (ly:assoc-get 'vformat opt (lambda (v)(format "~A" v)) #f))
        (pformat (ly:assoc-get 'pformat opt (lambda (v)(format "~A" v)) #f))
        (pathsep (ly:assoc-get 'pathsep opt "/" #f))
        (port (ly:assoc-get 'port opt (current-output-port))))
    (tree-walk-branch tree path
      (lambda (path k val)
        (format #t "[~A] ~A" (key tree) (string-join (map pformat path) pathsep 'infix) port)
        (if (and dval val) (begin
                            (display ": " port)
                            (display (vformat val) port)
                            ))
        (newline port)
        ) `(sort . ,dosort) `(sortby . ,sortby) `(empty . ,empty) )
    ))
(define-public (tree->string tree . opt)
  (call-with-output-string
   (lambda (port)
     (apply tree-display tree (assoc-set! opt 'port port))
     )))


(define-method (display (tree <tree>) port)
  (let ((tkey (key tree)))
    (tree-display tree)))

(define-public (tree? tree)(is-a? tree <tree>))
(define-public (tree-create . key)
  (let ((k (if (> (length key) 0)(car key) 'node)))
    (make <tree> #:key k)
    ))

(export tree-set!)
(export tree-merge!)
(export tree-get-tree)
(export tree-get)
(export tree-get-from-path)
(export tree-get-keys)
(export tree-dispatch)
(export tree-collect)
(export tree-walk)
(export tree-walk-branch)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; registry

(define-public (get-registry-val key . def) #f)
(define-public (set-registry-val key val) #f)
(define-public (display-registry) #f)
(let ((vals (tree-create 'registry)))
  (set! get-registry-val (lambda (key . def)
                           (let ((ret (tree-get vals (if (list? key) key (list key)))))
                             (if ret ret (if (> (length def) 0)(car def) #f)))))
  (set! set-registry-val (lambda (key val) (tree-set! vals (if (list? key) key (list key)) val)))
  (set! display-registry (lambda () (tree-display vals
                                      `(vformat .
                                         ,(lambda (v)
                                            (let ((str (if (markup? v)
                                                           (markup->string v)
                                                           (format "~A" v)
                                                           )))
                                              (if (> (string-length str) 79)
                                                  (string-append
                                                   (substring/read-only str 0 76) " ...") str)) )))))
  )

(define (not-null? val)(if val #t #f))

(define-public getRegistryVal (define-scheme-function (parser location key val)(list? not-null?)
                                (get-registry-val key)))
(define-public setRegistryVal (define-music-function (parser location key val)(list? not-null?)
                                (set-registry-val key val)
                                (make-music 'SequentialMusic 'void #t)))
