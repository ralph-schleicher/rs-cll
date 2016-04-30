;;; main.lisp --- program entry point.

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

(defun standalone-debugger (c hook)
  "Debugger hook function for catching errors.
When running in batch mode, terminate the program by calling
`exit-failure'.  Otherwise, enter the standard debugger."
  (declare (ignore hook))
  (when (standalone-program-p)
    (format *error-output*
	    "~&~A: internal error (~A)~%~A~&"
	    (program-invocation-short-name) (type-of c) c)
    (exit-failure)))

(export 'standalone-program)
(defun standalone-program ()
  "Disable features available in an interactive Lisp."
  (setf *standalone-program* t
	*debugger-hook* #'standalone-debugger)
  #+sbcl
  (setf	sb-ext:*muffled-warnings* 'warning)
  (values))

(export 'define-entry-point)
(defmacro define-entry-point (name (&optional (standalone-program t)) &body body)
  "Define a function where a program starts its execution.
Errors are trapped and printed to the *error-output* stream.
 "
  (multiple-value-bind (forms declarations doc-string)
      (alexandria:parse-body body :documentation t)
    (let ((arguments (gensym "ARGUMENTS")))
      `(defun ,name (&rest ,arguments)
	 ,@(when doc-string `(,doc-string))
	 (declare (ignore ,arguments))
	 ,@declarations
	 (let ((*standalone-program* ,standalone-program)
	       (*debugger-hook* #'standalone-debugger)
	       #+sbcl
	       (sb-ext:*muffled-warnings* 'warning))
	   (handler-bind ((serious-condition #'invoke-debugger))
	     ,@forms
	     (exit-success))
	   )))))

;;; main.lisp ends here
