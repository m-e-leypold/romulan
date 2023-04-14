;;; ------------------------------------------------------------------------*- common-lisp -*-|
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

(defsystem "de.m-e-leypold.romulan"
  :author "M E Leypold [elegant-weapons ( AT) m-e-leypold (DOT) de]"
  :licence "GPL3"
  :description "Declarative interface to clingon"
  :depends-on ("clingon")
  :components ((:file "romulan")))

(defsystem "de.m-e-leypold.romulan/tests"
  :author "M E Leypold [elegant-weapons ( AT) m-e-leypold (DOT) de]"
  :licence "GPL3"
  :depends-on ("de.m-e-leypold.romulan")
  :description "Tests and specifications for ROMULAN"
  :components ((:file "tests")))

(defsystem "de.m-e-leypold.romulan/prerequisites"
  :author "M E Leypold [elegant-weapons ( AT) m-e-leypold (DOT) de]"
  :licence "GPL3"
  :depends-on ("clingon")
  :description "Just all external prerequisites"
  :components ())

(defsystem "de.m-e-leypold.romulan/load-all"
  :author "M E Leypold [elegant-weapons ( AT) m-e-leypold (DOT) de]"
  :licence "GPL3"
  :description "Load all systems in ROMULAN"
  :depends-on ("de.m-e-leypold.romulan"))

