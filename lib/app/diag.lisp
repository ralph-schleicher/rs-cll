;;; diag.lisp --- diagnostic messages.

;; Copyright (C) 2011 Ralph Schleicher

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

(in-package :rs-cll)

(define-condition diagnostic-message (simple-condition)
  ((program-name
    :reader diagnostic-message-program-name
    :initarg :program-name
    :initform (program-invocation-short-name)
    :type (or null string pathname file-stream))
   (file-name
    :reader diagnostic-message-file-name
    :initarg :file-name
    :initform nil
    :type (or null string pathname file-stream))
   (line-number
    :reader diagnostic-message-line-number
    :initarg :line-number
    :initform nil
    :type (or null integer string))
   (level
    :reader diagnostic-message-level
    :initarg :level
    :initform :message
    :type (member :error :warning :message)))
  (:report
   (lambda (c stream)
     (let* ((program-name (let ((tem (diagnostic-message-program-name c)))
			    (if (or (null tem) (stringp tem))
				tem
			      (namestring tem))))
	    (file-name (let ((tem (diagnostic-message-file-name c)))
			 (if (or (null tem) (stringp tem))
			     tem
			   (namestring tem))))
	    (line-number (when file-name
			   (diagnostic-message-line-number c)))
	    (tag (case (diagnostic-message-level c)
		   (:warning
		    "warning")))
	    (message (apply #'format nil
			    (simple-condition-format-control c)
			    (simple-condition-format-arguments c))))
       ;; The message text should not begin with a capital letter when
       ;; it follows a program name or file name.
       ;;(if (or program-name file-name)
       ;;    (nstring-downcase message :end 1)
       ;;  (nstring-upcase message :end 1))
       (format stream
	       "~@[~A:~]~@[~A:~]~@[~A:~]~:[~; ~]~@[~A: ~]~A"
	       program-name file-name line-number
	       (or program-name file-name)
	       tag message))))
  (:documentation
   "Condition type for a diagnostic message.
Slot PROGRAM-NAME is the program name.  Default is the value returned
 by the ‘program-invocation-short-name’ function.  If nil, the program
 name is not part of the message text.
Slot FILE-NAME is the file name operated on.  Default nil, that means
 not applicable.
Slot LINE-NUMBER is the line number operated on.  Default nil, that means
 not applicable.  Value is only used if FILE-NAME is not null.
Slot LEVEL is the severity level.  Value is either :error, :warning, or
 :message.  Default is :message."))

(defun the-condition (datum &rest arguments)
  "Return the equivalent condition object for a condition designator.
If DATUM is a condition, the condition is the datum itself.  In that
case, no other arguments are permitted.  If DATUM is a symbol, the
condition is the result of

     (apply #'make-condition DATUM ARGUMENTS)

If DATUM is a string, the condition is the result of

     (make-condition 'diagnostic-message
                     :format-control DATUM
                     :format-arguments ARGUMENTS)

If DATUM is a cons cell, the condition is the result of

     (apply #'make-condition (car DATUM)
            :format-arguments (cdr DATUM)
            ARGUMENTS)

In that case, the condition type should have an appropriate default
format control string, for example

     (define-condition invalid-argument (diagnostic-message)
       ()
       (:default-initargs
        :format-control \"invalid argument '~A'\"))"
  (etypecase datum
    (condition
     (ensure-type arguments 'null)
     datum)
    (symbol
     (apply #'make-condition datum arguments))
    (string
     (make-condition 'diagnostic-message
		     :format-control datum
		     :format-arguments arguments))
    (cons
     (apply #'make-condition (car datum)
	    :format-arguments (cdr datum)
	    arguments))))

(defun say (datum &rest arguments)
  "Print a condition.

Argument is a condition designator.

Print the condition report to the ‘*standard-output*’ stream and return
the condition object."
  (let ((c (apply #'the-condition datum arguments)))
    (fresh-line *standard-output*)
    (princ c *standard-output*)
    (fresh-line *standard-output*)
    c))

(defun die (datum &rest arguments)
  "Print a condition and enter the debugger.

Argument is a condition designator.

Print the condition report to the ‘*error-output*’ stream and terminate
the program."
  (let ((c (apply #'the-condition datum arguments)))
    (fresh-line *error-output*)
    (princ c *error-output*)
    (fresh-line *error-output*)
    (exit-failure)))

;;; diag.lisp ends here
