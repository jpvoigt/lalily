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

(use-modules (lalily definitions))

(set-registry-val lalily:registry-verbose #t)

(set-registry-val lalily:paper:global-staff-size 16)
(set-registry-val lalily:paper:default '(lalily default))
(set-registry-val '(lalily layout default) '(lalily default))
(set-registry-val '(lalily midi default) '(lalily default))

(set-registry-val '(lalily store persons file) "persons.txt")

(set-registry-val '(lalily header copyright) #{ \markup { \with-url #"https://github.com/jpvoigt/lalily/" "Lilypond with lalily" } #})
