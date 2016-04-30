;;; work-dir.lisp --- working directory.

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

(defsubst posix-getcwd ()
  "Return the process's working directory.
Value is a pathname."
  (uiop:getcwd))

(defsubst posix-chdir (directory)
  "Set the process's working directory to DIRECTORY.
Value is the pathname of the new working directory."
  (uiop:chdir directory)
  (posix-getcwd))

;; Utilize a get/set pair of functions for the interface
;; because the return value of the setter may be different
;; from the argument value.

(export 'get-working-directory)
(defun get-working-directory ()
  "Return the process's working directory.
Value is a pathname.

Signal a file-error if the directory can not be determined."
  (handler-case
      (posix-getcwd)
    (serious-condition (c)
      (unless (signal (make-condition 'file-error))
	(error c)))))

(export 'set-working-directory)
(defun set-working-directory (directory &optional (default t))
  "Set the process's working directory to DIRECTORY.
Value is the pathname of the new working directory.

First argument DIRECTORY is either a string (interpreted as a
 directory file name) or a pathspec.
If optional second argument DEFAULT is true, adjust the special
 variable *default-pathname-defaults*, too.  This is the default.

Signal a file-error if the directory can not be changed."
  (let ((dir (handler-case
		 (posix-chdir directory)
	       (serious-condition (c)
		 (unless (signal (make-condition 'file-error :pathname directory))
		   (error c))))))
    (when (not (null default))
      (setf *default-pathname-defaults* dir))
    dir))

(export 'with-working-directory)
(defmacro with-working-directory ((directory &rest arg) &body body)
  "Temporarily change the process' working directory to DIRECTORY
and evaluate BODY."
  (let ((here (gensym)))
    `(let ((*default-pathname-defaults* *default-pathname-defaults*))
       (let ((,here (get-working-directory)))
       	 (set-working-directory ,directory ,@arg)
	 (unwind-protect
	      (progn ,@body)
	   (set-working-directory ,here nil))))))

;;; work-dir.lisp ends here
