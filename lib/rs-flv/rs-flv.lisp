;; rs-flv.lisp --- a recipe for file-local variables.

;; Copyright (C) 2014 Ralph Schleicher

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;;
;;    * Redistributions of source code must retain the above copyright
;;      notice, this list of conditions and the following disclaimer.
;;
;;    * Redistributions in binary form must reproduce the above copyright
;;      notice, this list of conditions and the following disclaimer in
;;      the documentation and/or other materials provided with the
;;      distribution.
;;
;;    * Neither the name of the copyright holder nor the names of its
;;      contributors may be used to endorse or promote products derived
;;      from this software without specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
;; FOR A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE
;; COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
;; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
;; BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
;; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
;; CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
;; LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
;; ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;; POSSIBILITY OF SUCH DAMAGE.

;;; Code:

(in-package :common-lisp-user)

(defpackage :rs-flv
  (:use :common-lisp)
  (:documentation "File-Local Variables

The ‘rs-flv’ library provides a recipe for file-local variables.
Typical usage is to wrap your code as follows:

     (eval-when (:compile-toplevel :load-toplevel :execute)
       (rs-flv:save-special-variables
        '((*read-default-float-format* double-float))))

     ;; Your code with many literal floating-point numbers goes here.

     (eval-when (:compile-toplevel :load-toplevel :execute)
       (rs-flv:restore-special-variables))

See <https://common-lisp.net/project/cdr/document/9/index.html> for
more details on the topic.  See ‘net.didierverna.asdf-flv’ for an
implementation of CDR 9 that depends on ASDF."))

(in-package :rs-flv)

(defvar special-variables-stack ()
  "Stack of saved special variables.")

(export 'save-special-variables)
(defun save-special-variables (bindings)
  "Save and optionally bind special variables.

Argument BINDINGS is a list of variable bindings.
Each element is either a symbol or a list of the form

     (SYMBOL NEW-VALUE)

With that, SYMBOL will be bound to NEW-VALUE after the current value
of SYMBOL has been saved.  Only special variables can be bound as
file-local variables.

You should call ‘restore-special-variables’ to restore the values
saved by the last call of ‘save-special-variables’."
  (push (mapcar (lambda (elem)
		  (multiple-value-bind (symbol new-value new-value-p)
		      (if (consp elem)
			  (progn
			    (when (/= (length elem) 2)
			      (error 'program-error))
			    (values (first elem) (second elem) t))
			(values elem))
		    (prog1
			;; Save symbol and it's value.
			(cons symbol (symbol-value symbol))
		      ;; Optionally set new value.
		      (when new-value-p
			(set symbol new-value)))))
		bindings)
	special-variables-stack))

(export 'restore-special-variables)
(defun restore-special-variables ()
  "Restore file-local variables saved by ‘save-special-variables’."
  (mapc (lambda (pair)
	  (let ((symbol (car pair))
		(value (cdr pair)))
	    (set symbol value)))
	(pop special-variables-stack)))

;; rs-flv.lisp ends here
