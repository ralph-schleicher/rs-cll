;;; temp-dir.lisp --- temporary files and directories.

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

(defun temp-dir (directory)
  (cond ((null directory)
	 *default-pathname-defaults*)
	((eq directory t)
	 (uiop:ensure-directory-pathname
	  (or (environment-variable "TMPDIR")
	      (environment-variable "TMP")
	      #+unix
	      "/tmp"
	      #+windows
	      (or (environment-variable "TEMP")
		  "C:\\Temp")
	      #-(or unix windows)
	      (fix-me 'temp-dir))))
	((stringp directory)
	 (uiop:ensure-directory-pathname directory))
	(t
	 (uiop:pathname-directory-pathname (pathname directory)))))

(export 'temporary-file-name)
(defun temporary-file-name (&key (prefix "temp") directory)
  "Generate a pathname that may be used for a temporary file.

Keyword argument PREFIX is the initial sequence of characters for the
 file name.  Default is \"temp\".
Keyword argument DIRECTORY specifies the directory in which the file
 name is created.  Value is either nil, t, a string, or a pathname
 designator.  A value of nil means to utilize the directory part of
 `*default-pathname-defaults*', t means to use some system specific
 temporary directory, a string is interpreted as a directory file
 name, and a pathname designator is used as is.

Value is a pathname whose file name is PREFIX followed by six random
characters (see the `random-string' function).

When keyword argument DIRECTORY is t, the system specific temporary
directory is chosen as follows.  First, the environment variables
'TMPDIR' and 'TMP' are examined in that order.  If both environment
variables are not set, further processing depends on the type of
operating system.  On Unix, fall back to the '/tmp' directory.  On
Windows, check the value of the environment variable 'TEMP', then
fall back to the 'C:\\Temp', directory."
  (declare (type (or null string) prefix))
  (let ((defaults (temp-dir directory)))
    (iter (for tem = (make-pathname
		      :name (concatenate 'string prefix (random-string 6))
		      :defaults defaults))
	  (unless (uiop:file-exists-p tem)
	    (return tem)))))

(export 'temporary-file)
(defun temporary-file (&key (prefix "temp") directory (direction :output) (element-type 'character) (external-format :default))
  "Create a unique file.

Keyword arguments PREFIX and DIRECTORY have the same meaning as for
 the `temporary-file-name' function.
Keyword arguments DIRECTION, ELEMENT-TYPE, and EXTERNAL-FORMAT have
 the same meaning as for the `open' function.

Value is a file stream to the newly created file."
  (declare (type (or null string) prefix))
  (let ((defaults (temp-dir directory)))
    (iter (for stream = (open (make-pathname
			       :name (concatenate 'string prefix (random-string 6))
			       :defaults defaults)
			      :direction direction
			      :element-type element-type
			      :if-exists nil
			      :if-does-not-exist :create
			      :external-format external-format))
	  (when (not (null stream))
	    (return stream)))))

(export 'with-temporary-file)
(defmacro with-temporary-file ((var &rest arg) &body body)
  "Create a temporary file and evaluate the body forms.

First argument VAR is the variable name to which the file stream of
 the temporary file is bound.
Remaining arguments are passed on to the `temporary-file' function.

When control leaves the body, either normally or abnormally, the
temporary file is automatically closed and deleted.

Value is the value of the last form of BODY."
  (let ((temp (gensym "temp")))
    `(let* ((,var (temporary-file ,@arg))
	    (,temp (pathname ,var)))
       (unwind-protect
	    (progn ,@body)
	 (when (ignore-errors (open-stream-p ,var))
	   (close ,var))
	 (ignore-errors (delete-file ,temp))))))

(export 'temporary-directory)
(defun temporary-directory (&key (prefix "temp") directory)
  "Create a unique directory.

Keyword arguments PREFIX and DIRECTORY have the same meaning as for
 the `temporary-file-name' function.

Value is the pathname to the newly created directory."
  (declare (type (or null string) prefix))
  (let ((defaults (temp-dir directory)))
    (let (dir created)
      (iter (for tem = (make-pathname
			:name (concatenate 'string prefix (random-string 6))
			:defaults defaults))
	    (multiple-value-setq (dir created)
	      (ensure-directories-exist (uiop:ensure-directory-pathname tem)))
	    (when (not (null created))
	      (return dir))))))

(export 'with-temporary-directory)
(defmacro with-temporary-directory ((var &rest arg) &body body)
  "Create a temporary directory and evaluate the body forms.

First argument VAR is the variable name to which the pathname of
 the temporary directory is bound.
Remaining arguments are passed on to the `temporary-directory'
 function.

When control leaves the body, either normally or abnormally, the
temporary directory is automatically deleted.  This includes all
files and directories within the temporary directory.

Value is the value of the last form of BODY."
  `(let ((,var (temporary-directory ,@arg)))
     (unwind-protect
	  (progn ,@body)
       (ignore-errors (delete-directory-and-files ,var)))))

;;; temp-dir.lisp ends here
