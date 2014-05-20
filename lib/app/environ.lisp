;;; environ.lisp --- environment variables.

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

(defun environ-alist-from-list (environ)
  "Convert a list of environment variables into an alist.
The function `environ-list-from-alist' does it the other way around."
  (iter (for var :in environ)
	(for pos = (position #\= var :test #'equal))
	(when pos
	  (for name = (subseq var 0 pos))
	  (for value = (subseq var (1+ pos)))
	  (collect (cons name value)))))

(defun environ-list-from-alist (environ)
  "Convert an alist of environment variables into a list.
The function `environ-alist-from-list' does it the other way around."
  (iter (for var :in environ)
	(for name = (car var))
	(for value = (cdr var))
	(when (and name value)
	  (collect (concatenate 'string name "=" value)))))

#+(and sbcl windows)
(progn
  ;; See 'sbcl-1.1.0/src/runtime/run-program.c'.
  (sb-alien:define-alien-routine msvcrt-environ (* sb-alien:c-string))
  (defun sbcl-windows-environ ()
    (sb-ext::c-strings->string-list (msvcrt-environ))))

(defsubst posix-environ ()
  "Return an alist of environment variables."
  #+clisp
  (ext:getenv)
  #+sbcl
  (environ-alist-from-list
   #+unix
   (sb-ext:posix-environ)
   #+windows
   (sbcl-windows-environ)
   #-(or unix windows)
   (fix-me 'posix-environ))
  #-(or clisp sbcl)
  (fix-me 'posix-environ))

(export 'environment-variables)
(defun environment-variables ()
  "Return all environment variables as an associated list.
List elements are cons cells of the form

     (NAME . VALUE)

where NAME and VALUE are the name respective value of an
environment variable."
  (posix-environ))

(defsubst posix-getenv (name)
  "Return the value of the environment variable NAME,
or nil if environment variable NAME is not defined."
  #+ccl
  (ccl:getenv name)
  #+clisp
  (ext:getenv name)
  #+sbcl
  (sb-ext:posix-getenv name)
  #-(or ccl clisp sbcl)
  (fix-me 'posix-getenv))

(defsubst posix-setenv (name value replace)
  "Set the value of the environment variable NAME to VALUE.
If the environment already contains an entry with key NAME and third
argument REPLACE is true, replace the entry with key NAME.  Otherwise,
do nothing.
Value is true on success, or nil if an error occurs."
  #+ccl
  (ccl:setenv name value replace)
  #+clisp
  (unless (and (ext:getenv name) (not replace))
    (setf (ext:getenv name) value))
  #+sbcl
  (handler-case
      (zerop (sb-posix:setenv name value (if replace 1 0)))
    (sb-posix:syscall-error ()))
  #-(or ccl clisp sbcl)
  (fix-me 'posix-setenv))

(defsubst posix-unsetenv (name)
  "Remove the entry with key NAME from the environment.
Value is true on success, or nil if an error occurs."
  #+clisp
  (setf (ext:getenv name) nil)
  #+sbcl
  (handler-case
      (zerop (sb-posix:unsetenv name))
    (sb-posix:syscall-error ()))
  #-(or clisp sbcl)
  (fix-me 'posix-unsetenv))

(export 'environment-variable)
(defun environment-variable (name)
  "Return the value of the environment variable NAME.
Value is nil if no entry with key NAME exists."
  (declare (type string name))
  (posix-getenv name))

(defun (setf environment-variable) (value name &optional (replace t))
  "Set the value of the environment variable NAME to VALUE.
If the environment already contains an entry with key NAME
 and optional argument REPLACE is true (this is the default),
 replace the entry with key NAME.  Otherwise, do nothing.
If VALUE is nil, remove the entry with key NAME from the
 environment."
  (declare (type (or null string) value))
  (declare (type string name))
  (if (not (null value))
      (posix-setenv name value replace)
    (posix-unsetenv name)))

;;; environ.lisp ends here
