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
   :commandline-interface
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


#-(or abcl clozure cmucl gcl lispworks mcl sbcl scl xcl)
(defconstant can-simulate-empty-argv nil)

#+(or abcl clozure cmucl gcl lispworks mcl sbcl scl xcl)
(progn
  (defconstant can-simulate-empty-argv t)
  (defmacro with-posix-args ((&rest args) &body body)
    `(let ((
	    #+abcl ext:*command-line-argument-list* ; Use 1.0.0 or later!
	    #+clozure ccl:*command-line-argument-list*
	    #+(or cmucl scl) extensions:*command-line-strings*
	    #+gcl si:*command-args*
	    #+lispworks sys:*line-arguments-list*
	    #+sbcl sb-ext:*posix-argv*
	    #+xcl system:*argv*
	    ,args))

       ;; Note: uiop only provides for reading the argv, but not for setting them. Unfortunately
       ;; clingon invokes uiop:command-line-arguments when no arguments are passed to
       ;; clingon:run. Which makes it impossible to invoke a clingon command directly with an
       ;; empty argument vector in a process that got any arguments. E.g. for testing.
       ;;
       ;; Without patching clingon the only reliable option to simulate a call with a specific
       ;; argument is to set the argument vector of the process temporarily and that is only
       ;; possible if it is a dynamic variable.
       ;;
       ;; The list above has been pertially ripped from uiop and only entries retained that are
       ;; dynamic variables. The mechanism has only been tested for sbcl so far.
       ;;
       ;; For the following lisp implementations I don't currently know a way to override the
       ;; command line argv. Long term changing clingon to allow overriding even for an empty
       ;; argv seems to be unavoidable.
       ;;
       ;; Unsupported on: allegro mkcl genera clisp clasp
       ;;

       ,@body)))

(defun run-with-argv (cmd argv)
  #+(or abcl clozure cmucl gcl lispworks mcl sbcl scl xcl)
  (with-posix-args (argv)
    (clingon-run cmd))
  #-(or abcl clozure cmucl gcl lispworks mcl sbcl scl xcl)
  (progn
    (assert argv nil "Cannot simulate empty argv in this implementation")
    (clingon-run cmd argv)))

;; Note: For the case that we don't have with-posix-argv, see above. In this case an empty argv
;; cannot be simulated. Tests using this need to be skipped. CAN-SIMULATE-EMPTY-ARGV can be
;; used to make this decision.

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

     (defun ,name (&optional (first 'process) &rest rest)
       ,(format nil "Command line interface '~A'" (symbol-name name))
       (case first
	 ('process (clingon:run ,name))
	 (:argv    (apply 'run-with-argv ,name rest))
	 (t       (apply 'run-with-argv ,name first rest))))))

(defun apply-command (cmd name varargsp key-params)
  (let ((pos-args (clingon:command-arguments cmd))
	(key-args (mapcar #'(lambda (key) (list key (clingon:getopt cmd key))) key-params)))
    (setf key-args (apply #'concatenate 'list key-args))
    (if varargsp
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


(defun define-command% (name procedure lambda-list definitions)

  (let ((varargs (getf definitions :varargs)))
    (remf definitions :varargs)
    (remf definitions :docstring)

    (setf definitions (make-clingon-command-definitions name definitions))

    (multiple-value-bind (pos-params key-params) (split-lambdalist lambda-list)
      (let ((keywords (find-package :keyword)))
	(setf key-params (mapcar #'(lambda (s) (intern (symbol-name s) keywords)) key-params)))

      (assert (or (not varargs) (= 1 (length pos-params)))
	      nil (format
		   nil
		   "varargs commands must only have on positional paramater, ~S has ~A"
		   name pos-params))

      (let ((command
	      (apply #'clingon:make-command
		     :handler #'(lambda (cmd)
				  (apply-command cmd procedure varargs key-params))
		     definitions)))
	command))))

(defmacro define-subcommand (name lambda-list (&rest definitions) &body body )
  `(progn
     (defun ,name ,lambda-list
       ,@body)
     (push (define-command% (quote ,name) (quote ,name) (quote ,lambda-list) (quote ,definitions))
	   (get *current-cli* 'sub-commands))))


(defun end-subcommand-interface ()
  (let ((definitions (symbol-value *current-cli*)))

    (setf definitions (make-clingon-command-definitions *current-cli* definitions))

    (setf (getf definitions :handler)
	  #'(lambda (cmd) (clingon:print-usage cmd *standard-output*)))
    (setf (getf definitions :sub-commands) (get *current-cli* 'sub-commands))

    (setf (symbol-value *current-cli*)
	  (apply 'clingon:make-command definitions)))
  (setf *current-cli* nil))

;;; * -- Single command interface ---------------------------------------------------------------------------|

(defmacro commandline-interface (name lambda-list description attributes &body body)
  (setf (getf attributes :description) description)
  (let ((procedure (gensym (concatenate 'string "G." (symbol-name name)))))
    `(progn

       (defun ,procedure ,lambda-list
	 ,@body)

       (declaim (special ,name))

       (setf ,name
	     (define-command% (quote ,name) (quote ,procedure) (quote ,lambda-list) (quote ,attributes)))

       (defun ,name (&optional (first 'process) &rest rest)

	 ;; TODO: This procedure should take key arguments for direct calling (apply) and simulating argv
	 ;; and use with-posix-argv

	 ;; TODO: Also use run and run-with-argv, the latter defaulting to clingon::run for
	 ;; implementations where I cannot have with-posix-argv. See there.

	 ,(format nil "Command line interface '~A'" (symbol-name name))
	 ;; (clingon:run ,name)
	 (case first
		('process (clingon:run ,name))
		(:apply  (apply #',procedure rest))
		(:argv    (apply 'run-with-argv ,name rest))
		(t       (apply 'run-with-argv ,name first rest)))))))
