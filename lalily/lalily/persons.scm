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

(define-module (lalily persons))

(use-modules  (lily)(lalily store)(lalily laly)(lalily lascm)
	      (oop goops)(ice-9 rdelim))

(define-class <person> ()
  (key #:accessor person-key #:init-keyword #:key)
  (name #:accessor person-name #:init-keyword #:name)
  (life #:accessor person-life #:init-keyword #:life)
)
(export person-key)
(export person-name)
(export person-life)
(define-public (person-create key name life)(make <person> #:key key #:name name #:life life))
(define-public (person? p)(is-a? p <person>))
(define-method (display (person <person>) port)
               (format #t "~A: ~A ~A" (person-key person) (person-name person) (person-life person) port))

(define-class <person-db> ())
(define-method (register-person! (db <person-db>) sym name life)
  (ly:message "no implementation for ~A" (class-name (class-of db))))
(define-method (get-person (db <person-db>) sym)
  (ly:message "no implementation for ~A" (class-name (class-of db))))
(define-method (display-persons (db <person-db>))
  (ly:message "no implementation for ~A" (class-name (class-of db))))
(export register-person!)
(export get-person)
(export display-persons)
(define-public (person-db? pdb)(is-a? pdb <person-db>))

(define-class <person-alist> (<person-db>)
  (persons #:accessor person-alist #:init-value '()))
(define-method (register-person! (db <person-alist>) sym name life)
  (set! (person-alist db) (assoc-set! (person-alist db) sym (make <person> #:key sym #:name name #:life life)) ) )
(define-method (get-person (db <person-alist>) sym)
  (assoc-get sym (person-alist db)))
(define-method (display-persons (db <person-alist>))
  (for-each (lambda (p)
                    (let ((person (cdr p)))
                         (format #t "[person] (~A) ~A ~A\n" (person-key person) (person-name person) (person-life person))))
            (sort (person-alist db) (lambda (p1 p2) (string-ci<? (person-life (cdr p1)) (person-life (cdr p2)))))))
(define-public (person-alist-create)(make <person-alist>))

(define-method (import-persons (db <person-db>) (file <string>))
  (let ((fl (la:find-file file)))
       (if (not (file-exists? fl)) (set! fl (%search-load-path file)))
       (if (file-exists? fl)
           (with-input-from-file fl (lambda ()
                     (do ((line "" (read-line) ))
                         ((eof-object? line) fl)
                         (let ((plst (string-split line #\|)))
                              (if (>= (length plst) 3)
                                  (register-person! db (string->symbol (car plst)) (cadr plst) (caddr plst))
                         ))
))))))
(export import-persons)

(define-public (get-person-store) #f)
(define-public (set-person-store! pst) #f)
(let ((person-db #f))
     (set! get-person-store (lambda () 
        (if (person-db? person-db) person-db
            (let ((db (person-alist-create))
                  (pdbname (get-registry-val '(lalily store persons file))))
                 (if (not pdbname) (set! pdbname "lalily-persons.txt"))
                 (import-persons db pdbname)
                 (set! person-db db)
                 db) )))
     (set! set-person-store! (lambda (db) (set! person-db db)))
)
