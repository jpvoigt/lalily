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

(use-modules (lalily latex))

; pdflatex markup-list command
(define-markup-list-command (pdflatex layout props m)(markup-list?)
  (tex-markup-list layout props
    `("\\usepackage[utf8]{inputenc}") "pdflatex" "-interaction=batchmode" m))
; pdflatex markup-list include command
(define-markup-list-command (pdflatexInclude layout props m)(string?)
  (tex-markup-list layout props
    `("\\usepackage[utf8]{inputenc}") "pdflatex" "-interaction=batchmode" m))

; xelatex markup-list command
(define-markup-list-command (xelatex layout props m)(markup-list?)
  (let ((font-name (chain-assoc-get 'font-name props #f)))
    (if (not (string? font-name))
        (begin
         (set! font-name (chain-assoc-get 'font-family props #f))
         (if (string? font-name)
             (ly:warning "using deprecated property 'font-family for '~A'" font-name)
             (begin
              (set! font-name (get-registry-val lalily:latex:default-font "DejaVu Serif"))
              (ly:warning "no font-name defined! trying '~A' ..." font-name)
              ))
         ))
    (tex-markup-list layout props
      `("\\usepackage[T1]{fontenc}" "\\usepackage{fontspec}"
         "\\defaultfontfeatures{Mapping=tex-text}"
         ,(format "\\setmainfont{~A}" font-name)
         ,@(chain-assoc-get 'packages props '())
         )
      "xelatex" "-interaction=batchmode" m)
    ))
; xelatex markup-list include command
(define-markup-list-command (xelatexInclude layout props m)(string?)
  (let ((font-name (chain-assoc-get 'font-name props #f)))
    (if (not (string? font-name))
        (begin
         (set! font-name (chain-assoc-get 'font-family props #f))
         (if (string? font-name)
             (ly:warning "using deprecated property 'font-family for '~A'" font-name)
             (begin
              (set! font-name (get-registry-val lalily:latex:default-font "DejaVu Serif"))
              (ly:warning "no font-name defined! trying '~A' ..." font-name)
              ))
         ))
    (tex-markup-list layout props
      `("\\usepackage[T1]{fontenc}" "\\usepackage{fontspec}"
         "\\defaultfontfeatures{Mapping=tex-text}"
         ,(format "\\setmainfont{~A}" font-name)
         ,@(chain-assoc-get 'packages props '())
         )
      "xelatex" "-interaction=batchmode" m)))

