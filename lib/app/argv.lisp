;;; argv.lisp --- program arguments.

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

(defsubst posix-argv ()
  "Return a list of command line arguments according to POSIX.
That means the first element is the executable file name and
the remaining elements are the command line arguments."
  #+ccl
  ccl:*command-line-argument-list*
  #+clisp
  (ext:argv)
  #+ecl
  (ext:command-args)
  #+sbcl
  sb-ext:*posix-argv*
  #-(or ccl clisp ecl sbcl)
  (fix-me 'posix-argv))

(defvar *program-invocation-name* nil
  "The program name as invoked on the command line.")

(defvar *program-invocation-short-name* nil
  "Like `*program-invocation-name*' but without the directory part.")

(export 'program-invocation-name)
(defun program-invocation-name ()
  "Return the program name as invoked on the command line.

Value is a string."
  (when (null *program-invocation-name*)
    (setf *program-invocation-name* (first (posix-argv))))
  *program-invocation-name*)

(defun (setf program-invocation-name) (value)
  "Set the program name.

Value has to be a string, a pathname, or a file stream."
  (ensure-type value '(or string pathname file-stream))
  (setf *program-invocation-name* (namestring value)
	*program-invocation-short-name* nil))

(export 'program-invocation-short-name)
(defun program-invocation-short-name ()
  "Return the program name as invoked on the command line
but without the directory part.

Value is a string."
  (when (null *program-invocation-short-name*)
    (setf *program-invocation-short-name* (file-namestring (program-invocation-name))))
  *program-invocation-short-name*)

(defvar *program-arguments* nil
  "The list of program arguments.")

(export 'program-arguments)
(defun program-arguments ()
  "Return the list of program arguments.

Value is a list of strings."
  (when (null *program-arguments*)
    (setf *program-arguments* (rest (posix-argv))))
  *program-arguments*)

(defun (setf program-arguments) (value)
  "Set the list of program arguments.

Value has to be a list of strings."
  (ensure-type value 'list-of-strings)
  (setf *program-arguments* value))

;;; argv.lisp ends here
