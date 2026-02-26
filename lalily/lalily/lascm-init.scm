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
(re-export-module '(lalily lascm))

(define-public assocGet (define-scheme-function (l k)(list? symbol?)
    (assoc-get k l #f #f)))
(define-public assocSet (define-scheme-function (l k v)(list? symbol? scheme?)
    (assoc-set! l k v)))
(define-public assocSetAll (define-scheme-function (l v)(list? list?)
    (assoc-set-all! l v)))


