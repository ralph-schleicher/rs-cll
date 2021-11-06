;;; exit.lisp --- program termination.

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

(defsubst posix-exit (status)
  "Terminate the program and return STATUS back to the parent process.
Argument STATUS has to be an integer in the range from 0 to 255."
  (declare (type (integer 0 255) status))
  #+ccl
  (ccl:quit status)
  #+clisp
  (ext:quit status)
  #+ecl
  (ext:quit status)
  #+sbcl
  (let (x)
    (cond ((setf x (find-symbol "EXIT" :sb-ext))
	   (funcall x :code status))
	  ((setf x (find-symbol "QUIT" :sb-ext))
	   (funcall x :unix-status status))
	  (t
	   (fix-me 'posix-exit))))
  #-(or ccl clisp ecl sbcl)
  (fix-me 'posix-exit))

(defconst exit-success (progn
			 #+(or unix windows)
			 0
			 #-(or unix windows)
			 (fix-me 'exit-success))
  "Status code to indicate successful completion.")

(defun exit-success (&optional (status exit-success))
  "Terminate the program indicating successful completion."
  (ensure-type status '(integer 0 255))
  (if *standalone-program*
      (posix-exit status)
    (abort (make-condition
	    'simple-condition
	    :format-control "Program terminated with status code '~A'."
	    :format-arguments (list status)))))

(defconst exit-failure (progn
			 #+(or unix windows)
			 1
			 #-(or unix windows)
			 (fix-me 'exit-failure))
  "Status code to indicate a failure condition.")

(defun exit-failure (&optional (status exit-failure))
  "Terminate the program indicating a failure condition."
  (ensure-type status '(integer 0 255))
  (if *standalone-program*
      (posix-exit status)
    (abort (make-condition
	    'simple-condition
	    :format-control "Program terminated with status code '~A'."
	    :format-arguments (list status)))))

;;; exit.lisp ends here
