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

;;; * Define package ----------------------------------------------------------------------------------------|

(defpackage :de.m-e-leypold.romulan/tests
  (:documentation "Testing romulan")
  (:use
   :common-lisp
   :de.m-e-leypold.romulan))

(in-package :de.m-e-leypold.romulan/tests)

;;; * Some test fixtures -----------------------------------------------------------------------------------|

(commandline-interface romulan-test (words &key user verbose)

    "shouts back anything you write"

  (:usage       "[options ...] [arguments ...]"
   :varargs     t

   :options    (:user    (:description "user to greet"
			  :short-name #\u
			  :env-vars ("USER"))

			 :verbose (:type :counter
				   :description "verbosity level, every -v bumps up the verbosity by 1"
				   :short-name #\v
				   :long-name "verbose")))     ; <-- This would be the default

  (if (< 1 verbose)
      (format t "DEBUG: verbosity => ~S~%" verbose))

  (if (< 0 verbose)
      (format t "Hello, ~A!~%" user))

  (format t "~{~A~^ ~}!!!~%" (mapcar #'string-upcase words)))
