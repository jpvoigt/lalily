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

;; Guile 3 / LP 2.24: (re-export sym) doesn't work outside define-module.
;; Use module-re-export! instead, which re-exports all public symbols
;; from a module so they become visible in child modules (e.g. \paper { } blocks).
(define (re-export-module mod-name)
  (let* ((cur (current-module))
         (syms '()))
    (module-for-each
     (lambda (sym var)
       ;; Only re-export symbols that aren't locally defined in cur
       ;; to avoid "re-exporting local variable" errors in Guile 3
       (if (not (module-locally-bound? cur sym))
           (set! syms (cons sym syms))))
     (module-public-interface (resolve-module mod-name)))
    (if (not (null? syms))
        (module-re-export! cur syms))))

(use-modules (lalily definitions))
(re-export-module '(lalily definitions))

(load-from-path "lalily/lascm-init.scm")
(load-from-path "lalily/laly-init.scm")
(load-from-path "lalily/store-init.scm")
(load-from-path "lalily/markup-init.scm")
(load-from-path "lalily/latex-init.scm")
(load-from-path "lalily/lyrics-init.scm")
(load-from-path "lalily/persons-init.scm")
(load-from-path "lalily/edition-init.scm")

(let* ((cfn "lalily/config.scm")(config (%search-load-path cfn)))
  (if (and (string? config)(file-exists? config)) (load-from-path cfn)))

(if (lalily:verbose)(ly:message "lalily V ~A @ ~A" (glue-list lalily:version ".") (strftime "%d.%m.%Y %H:%M:%S" lalily:init)))
