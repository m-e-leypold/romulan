;;; Romulan - Declarative interface to the clingon command line argument parser.
;;; Copyright (C) 2023  M E Leypold
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <https://www.gnu.org/licenses/>.
;;;

;;; * Options -----------------------------------------------------------------------------------------------|

(declaim (optimize (speed 0) (space 0) (compilation-speed 0) (debug 3) (safety 3)))

;;; * Load system to be tested & the tests ------------------------------------------------------------------|

(asdf:load-system "de.m-e-leypold.romulan/tests")

;;; * define sandbox for tests ------------------------------------------------------------------------------|

(defpackage :de.m-e-leypold.cl-specification/run-tests
  (:documentation "Sandbox for testing romulan")
  (:use
   :common-lisp
   :de.m-e-leypold.romulan/tests))

(in-package :de.m-e-leypold.cl-specification/run-tests)

;;; * Actually executing the tests --------------------------------------------------------------------------|
