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
;;; * -- Options --------------------------------------------------------------------------------------------|
;;;
;;;   Will be changed to defaults when cl-specification has reached sufficient maturity.

(declaim (optimize (speed 0) (debug 3) (safety 3)))

;;; * -- Package definition & documentation -----------------------------------------------------------------|

(defpackage :de.m-e-leypold.romulan
  (:documentation "
    TODO: Complete package docstring
   ")
  (:use :common-lisp)
  (:export
   :commandline-subcommand-interface :end-subcommand-interface :define-subcommand
   :with-posix-args))

(in-package :de.m-e-leypold.romulan)

;;; * -- Utilities ------------------------------------------------------------------------------------------|

(defmacro do-plist ((key value plist) &body body)
  (let ((rest (gensym "G.rest.")))
    `(do* ((,rest  ,plist (cddr ,rest))
	   (,key   (car ,plist) (car ,rest))
	   (,value (cadr ,plist) (cadr ,rest)))
	 ((not ,rest))
       ,@body)))


(defun split-lambdalist (lambda-list)
  (let* ((posargs '())
	 (keyargs
	   (do ((param (car lambda-list) (car rest))
		(rest (cdr lambda-list) (cdr rest)))
	       ((or (not rest) (eq param '&key))
		rest)
	     (push param posargs))))
    (values posargs keyargs)))


(defmacro with-posix-args ((&rest args) &body body)
  `(let ((
	  #+sbcl sb-ext:*posix-argv*
	  ,args))
     ,@body))

(defmacro set-default (plist-place indicator default)
  (let ((value (gensym "G.value.")))
    `(let ((,value (getf ,plist-place ,indicator 'none)))
       (if (not ,value)
	   (remf ,plist-place ,indicator)
	   (if (eq 'none ,value)
	       (setf (getf ,plist-place ,indicator) ,default))))))

;;; * -- Interfacing to clingon -----------------------------------------------------------------------------|


(defun make-option (key attributes)
  ;; TODO Defaults for type and long

  (set-default attributes :type :string)
  (set-default attributes :long-name (string-downcase (symbol-name key)))

  (let ((type (getf attributes :type)))
    (remf attributes :type)
    (apply #'clingon:make-option `(,type :key ,key ,@attributes))))

(defun make-options (definitions)

  (let ((options '()))
    (do-plist (key attributes definitions)
      (assert (not (getf attributes :key)))
      (push (make-option key attributes) options))
    (reverse options)))


;;; * -- Subcommand Interface -------------------------------------------------------------------------------|

(defvar *current-cli* nil)

(defmacro commandline-subcommand-interface (name description &body attributes)
  (setf *current-cli* name)
  (setf (getf attributes :description) description)
  `(progn
     (declaim (special ,name))
     (setf ,name (quote ,attributes))
     (setf (get (quote ,name) 'sub-commands) '())
     (defun ,name (&rest argv)
       ""
       (clingon:run ,name argv))))


(defun apply-command (cmd name varargsp pos-params key-params)
  (let ((pos-args (clingon:command-arguments cmd))
	(key-args (mapcar #'(lambda (key) (list key (clingon:getopt cmd key))) key-params)))
    (setf key-args (apply #'concatenate 'list key-args))
    (if varargsp
	;; TODO: Assert for length of pos-params, maybe reduce to pos-params-count
	(apply name pos-args key-args)
	(apply name (concatenate 'list pos-args key-args)))))

(defun make-clingon-command-definitions (name definitions)
  (setf (getf definitions :options) (make-options (getf definitions :options)))
  (set-default definitions :name (string-downcase (symbol-name name)))
  (set-default definitions :usage "")

  (let ((description (getf definitions :description)))

    ;; Not useful ATM
    #+nil (if (consp description)
	      (setf (getf definitions :description) (format nil "~{~A~^~%~}" description)))
    )
  definitions)


(defun define-command% (name lambda-list definitions)

  (let ((varargs (getf definitions :varargs)))
    (remf definitions :varargs)
    (remf definitions :docstring)

    (setf definitions (make-clingon-command-definitions name definitions))


    (multiple-value-bind (pos-params key-params) (split-lambdalist lambda-list)
      (let ((keywords (find-package :keyword)))
	(setf key-params (mapcar #'(lambda (s) (intern (symbol-name s) keywords)) key-params)))

      (assert (or (not varargs) (= 1 (length pos-params))))

      (let ((command
	      (apply #'clingon:make-command
		     :handler #'(lambda (cmd)
				  (apply-command cmd name varargs pos-params key-params))
		     definitions)))
	(push command (get *current-cli* 'sub-commands))))))


(defmacro define-subcommand (name lambda-list (&rest definitions) &body body )
  `(progn
     (defun ,name ,lambda-list
       ,@body)
     (define-command% (quote ,name) (quote ,lambda-list) (quote ,definitions))))


(defun end-subcommand-interface ()
  (let ((definitions (symbol-value *current-cli*)))

    (setf definitions (make-clingon-command-definitions *current-cli* definitions))

    (setf (getf definitions :handler)
	  #'(lambda (cmd) (clingon:print-usage cmd *standard-output*)))
    (setf (getf definitions :sub-commands) (get *current-cli* 'sub-commands))

    (setf (symbol-value *current-cli*)
	  (apply 'clingon:make-command definitions)))
  (setf *current-cli* nil))
