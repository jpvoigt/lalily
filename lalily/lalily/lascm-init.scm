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

(use-modules (lalily lascm))

(re-export glue-list)
(re-export glue-symbol)
(re-export format-alist)
(re-export assoc-set-all!)

(define-public assocGet (define-scheme-function (parser location l k)(list? symbol?)
    (assoc-get k l #f #f)))
(define-public assocSet (define-scheme-function (parser location l k v)(list? symbol? scheme?)
    (assoc-set! l k v)))
(define-public assocSetAll (define-scheme-function (parser location l v)(list? list?)
    (assoc-set-all! l v)))

(re-export rcdr)
(re-export rcddr)
(re-export rcdddr)
(re-export rcddddr)
(re-export rcar)
(re-export rcadr)
(re-export rcaddr)
(re-export rcadddr)

(re-export normalize-path-list)
(re-export normalize-path-string)

(re-export stack-create)
(re-export push)
(re-export get)
(re-export pop)
(re-export store)
(re-export name)

(re-export tree-create)
(re-export tree?)
(re-export tree-display)
(re-export tree-set!)
(re-export tree-merge!)
(re-export tree-get-tree)
(re-export tree-get)
(re-export tree-get-from-path)
(re-export tree-get-keys)
(re-export tree-dispatch)
(re-export tree-collect)
(re-export tree-walk)
(re-export tree-walk-branch)

(re-export get-registry-val)
(re-export set-registry-val)
(re-export display-registry)
(re-export getRegistryVal)
(re-export setRegistryVal)

(re-export with-append-file)
