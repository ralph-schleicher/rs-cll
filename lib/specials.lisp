;;; specials.lisp --- special variables.

;; Copyright (C) 2013 Ralph Schleicher

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
;;    * The name of the author may not be used to endorse or promote
;;      products derived from this software without specific prior
;;      written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR "AS IS" AND ANY EXPRESS
;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;; ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT,
;; INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
;; SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
;; HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
;; STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING
;; IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;; POSSIBILITY OF SUCH DAMAGE.

;;; Code:

(in-package :rs-cll)

(export 'optimize-for-speed)
(defvar optimize-for-speed
  '(optimize speed (safety 0) (space 0) (debug 1) (compilation-speed 0))
  "Optimization qualities for generating fast code.
You have to use this variable via

     (declare #.optimize-for-speed)")

(defvar special-variables-stack ()
  "Stack of saved special variables.")

(export 'save-special-variables)
(defmacro save-special-variables (&rest variables)
  "Save values of special variables.

Each argument is either a symbol or a list of the form

     (SYMBOL NEW-VALUE)

With that, SYMBOL will be bound to NEW-VALUE after the current
value of SYMBOL has been saved.

You can call `restore-special-variables' to restore the values
saved by the last call of `save-special-variables'.  Typical
usage is to wrap your code as follows:

     (eval-when (:compile-toplevel :load-toplevel :execute)
       (save-special-variables
        (*read-default-float-format* 'double-float)))

     ;; Your code with many literal floating-point numbers goes here.

     (eval-when (:compile-toplevel :load-toplevel :execute)
       (restore-special-variables))"
  (let ((symbol (gensym "SYM"))
	(value (gensym "VAL"))
	(define (gensym "DEF"))
	(arg (gensym "ARG"))
	(cache (gensym)))
    `(let ((,cache (iter (for ,arg :in (quote ,variables))
			 (if (consp ,arg)
			     (progn
			       (when (/= (length ,arg) 2)
				 (error 'program-error))
			       (setf ,symbol (first ,arg)
				     ,value (second ,arg)
				     ,define t))
			   (setf ,symbol ,arg
				 ,define nil))
			 ;; Save symbol and it's value.
			 (collect ,symbol)
			 (collect (symbol-value ,symbol))
			 ;; Set new value.
			 (when ,define
			   (set ,symbol (eval ,value))))))
       (push ,cache special-variables-stack)
       ,cache)))

(export 'restore-special-variables)
(defun restore-special-variables ()
  "Restore special variables saved by `save-special-variables'."
  (let ((cache (pop special-variables-stack)))
    (iter (while cache)
	  (for symbol = (pop cache))
	  (for value = (pop cache))
	  (set symbol value)))
  (values))

;;; specials.lisp ends here
