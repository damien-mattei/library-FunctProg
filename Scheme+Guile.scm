;; Scheme+.scm

;; version 1.0

;; author: Damien MATTEI

;; location: France

;; date: 2021

;; Guile Scheme version

;; Copyright 2021 Damien MATTEI

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;  for curly infix notation put in your .guile:
;; (read-enable 'curly-infix)

;; 
;; use :
;; (use-modules (Scheme+Guile))

;; or if in a guile subdirectory use it with (use-modules (guile Scheme+Guile.scm))
;; note that the above syntax means Scheme+Guile.scm must be in a directory named guile
;; and that you have launched Guile in the directory containing the guile subdirectory


;;(define-module (guile Scheme+)



(define-module (Scheme+Guile)

  #:export (def $bracket-apply$ <- -> <+ declare $ § condx <> )
  
  ;; #:replace ((new-lambda . lambda)
  ;; (new-cond . cond)
  ;;          )

  )

;;(use-modules (system syntax))

(include "def.scm")
(include "Scheme+Guile/array-square-brackets.scm")
(include "Scheme+Guile/assignment.scm")
(include "declare.scm")

(include "condx.scm")
(include "block.scm")
(include "not-equal.scm")

