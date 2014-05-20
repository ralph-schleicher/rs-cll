;;; exec-clisp.lisp --- run an external program.

;; Copyright (C) 2012 Ralph Schleicher

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

(defmethod %execute-program (program arguments &key input if-input-does-not-exist output if-output-exists error if-error-exists wait)
  (cond ((eq input t)
	 (setf input :terminal))
	((or (stringp input) (pathnamep input))
	 (let ((s (open input :direction :input :if-does-not-exist if-input-does-not-exist)))
	   (when (null s)
	     (return-from %execute-program))
	   (close s))))
  (when (eq if-output-exists :supersede)
    (setf if-output-exists :overwrite))
  (let ((ans (multiple-value-list
	      (ext:run-program program
			       :arguments arguments
			       :wait wait
			       :input input
			       :output output
			       :if-output-exists if-output-exists))))
    (apply #'make-instance '%program
	   (cond (wait
		  ;; A value of nil means an exit status of zero.
		  (list :exit-status (or (first ans) 0)))
		 ((= (length ans) 1)
		  (list :input-stream (when (eq input :stream) (first ans))
			:output-stream (when (eq output :stream) (first ans))))
		 (t
		  ;; Close bidirectional stream.
		  (close (first ans))
		  (list :input-stream (second ans)
			:output-stream (third ans)))))))

;;; exec-clisp.lisp ends here
