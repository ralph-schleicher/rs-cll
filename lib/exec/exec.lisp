;;; exec.lisp --- run an external program.

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

(defparameter *pointer-size*
  (progn
    #+sbcl
    (sb-alien:alien-size (* sb-alien:char) :bytes)
    #-(or sbcl)
    8)
  "The size of a pointer.")

(defparameter *posix-arg-max*
  (progn
    #+aix
    24576
    #+bsd
    10240
    #+hpux
    20478
    #+linux
    131072
    #+osf1
    38912
    #+sunos
    10240
    #+windows
    32768
    #-(or aix bsd hpux linux osf1 sunos windows)
    4096)
  "The maximum length of the argument list for running a program,
including environment variables.  A value of nil means there is
no limit.

Try the shell command 'getconf ARG_MAX', to get the actual limit
for your system.")

(defclass %program ()
  ((input-stream
    :accessor %program-input-stream
    :initarg :input-stream)
   (output-stream
    :accessor %program-output-stream
    :initarg :output-stream)
   (error-stream
    :accessor %program-error-stream
    :initarg :error-stream)
   (exit-status
    :accessor %program-exit-status
    :initarg :exit-status))
  (:documentation
   "Default class representing the properties of an external program."))

(export 'program-input-stream)
(defgeneric program-input-stream (self)
  (:documentation
   "The input stream of an external program, or nil.
The Lisp process can write to the program's input stream.")
  (:method ((self %program))
    (%program-input-stream self)))

(export 'program-output-stream)
(defgeneric program-output-stream (self)
  (:documentation
   "The output stream of an external program, or nil.
The Lisp process can read from the program's output stream.")
  (:method ((self %program))
    (%program-output-stream self)))

(export 'program-error-stream)
(defgeneric program-error-stream (self)
  (:documentation
   "The error stream of an external program, or nil.
The Lisp process can read from the program's error stream.")
  (:method ((self %program))
    (%program-error-stream self)))

(export 'program-exit-status)
(defgeneric program-exit-status (self)
  (:documentation
   "The exit value of an external program or the negative signal value, or nil.")
  (:method ((self %program))
    (%program-exit-status self)))

(defgeneric %execute-program (program arguments &key input if-input-does-not-exist output if-output-exists error if-error-exists wait)
  (:documentation
   "Run an external program.

First argument PROGRAM is the absolute program file name (a string).
Second argument ARGUMENTS are the program arguments (a list of strings).
Keyword arguments have values as documented by the `execute-program'
 function.

Value is an object representing the properties of the external program.
Signal an error if PROGRAM can not be executed.")
  (:method (program arguments &rest rest)
    (declare (ignore program arguments rest))
    (error "The '%execute-program' function is not defined for ~A.~%~
Please send patches to <~A@~A>."
	   (lisp-implementation-type)
	   "rs" "ralph-schleicher.de")))

(export 'execute-program)
(defun execute-program (program &optional arguments &key extra-arguments (input t) (if-input-does-not-exist :error) (output t) (if-output-exists :error) (error t) (if-error-exists :error) (wait t))
  "Run an external program.

First argument PROGRAM is the program file name.  Value is either a
 string or a pathname.  If PROGRAM is an absolute or explicit relative
 file name, execute the specified file.  Otherwise, search for it in
 the standard program search path.
Second argument ARGUMENTS are the program arguments.  Value is a list
 of strings.
Keyword argument EXTRA-ARGUMENTS are additional program arguments.
 Value is a list of strings.  These arguments are appended to the
 normal ARGUMENTS in a way similar to the 'xargs' utility.  Use of
 this keyword may result in multiple invocations of PROGRAM.
Keyword argument INPUT is the source for the program's standard input
 stream.  Value is either nil, t, :stream, a string, or a pathname.
 Default is t and :stream is only valid if WAIT is nil.
Keyword argument IF-INPUT-DOES-NOT-EXIST specifies what to do if INPUT
 names a non-existing file.  Value is either :error, :create, or nil.
 Default is :error.
Keyword argument OUTPUT is the destination for the program's standard
 output stream.  Value is either nil, t, :stream, a string, or a
 pathname.  Default is t and :stream is only valid if WAIT is nil.
Keyword argument IF-OUTPUT-EXISTS specifies what to do if OUTPUT names
 an existing file.  Value is either :error, :supersede, :append, or
 nil.  Default is :error.
Keyword argument ERROR is the destination for the program's standard
 error stream.  Value is either nil, t, :stream, a string, or a
 pathname.  Default is t and :stream is only valid if WAIT is nil.
Keyword argument IF-ERROR-EXISTS specifies what to do if ERROR names
 am existing file.  Value is either :error, :supersede, :append, or
 nil.  Default is :error.
If keyword argument WAIT is true, block the Lisp process and wait for
 the program to terminate.  Otherwise run the program asynchronously.
 Default is true.

If INPUT names a non-existing file and IF-INPUT-DOES-NOT-EXIST is nil,
value is nil (no error).  Likewise if OUTPUT/ERROR names an existing
file and IF-OUTPUT-EXISTS/IF-ERROR-EXISTS is nil.  Otherwise, the
return value depends on the WAIT flag.  If WAIT is true, value is the
program's exit status.  Otherwise, value is an object representing the
external program.

For INPUT, OUTPUT, and ERROR, a value of nil means that the stream is
redirected to the null device, t means to inherit the stream from the
Lisp process, :stream means to create a new stream (only valid if WAIT
is nil), and a string or a pathname names a file.

For IF-INPUT-DOES-NOT-EXIST, a value of :error means to signal a file
error, :create means to create the file, and nil means to fail silently.

For IF-OUTPUT-EXISTS and IF-ERROR-EXISTS, a value of :error means to
signal a file error, :supersede means to create a new file with the
same name, :append means to modify the existing file at the end, and
nil means to fail silently."
  ;; Check arguments.
  (if (pathnamep program)
      (setf program (namestring program))
    (ensure-type program 'string))
  (ensure-type arguments 'list-of-strings)
  (ensure-type extra-arguments 'list-of-strings)
  (when extra-arguments
    (ensure-type wait '(not null)))
  (let ((stream-spec `(or (member nil t ,@(unless wait (list :stream))) string pathname))
	(input-spec '(member :error :create nil))
	(output-spec '(member :error :supersede :append nil)))
    (ensure-type input stream-spec)
    (ensure-type if-input-does-not-exist input-spec)
    (ensure-type output stream-spec)
    (ensure-type if-output-exists output-spec)
    (ensure-type error stream-spec)
    (ensure-type if-error-exists output-spec))
  (when extra-arguments
    (if (null *posix-arg-max*)
	(setf arguments (append arguments extra-arguments))
      (let* (;; Effectively usable space.
	     (usable (- *posix-arg-max*
			;; Size of argument vector, that is program
			;; name, arguments, and trailing null pointer.
			*pointer-size* (length program) 1
			(apply #'+ (mapcar (lambda (arg)
					     (+ *pointer-size*
						(length arg) 1))
					   arguments))
			*pointer-size*
			;; Size of environment variables, that is
			;; name, '=' character, value, and trailing
			;; null pointer.
			(apply #'+ (mapcar (lambda (var)
					     (+ *pointer-size*
						(length (car var)) 1
						(length (cdr var)) 1))
					   (environment-variables)))
			*pointer-size*
			;; Extra space.
			(min (ceiling (/ *posix-arg-max* 8)) 2048)))
	     ;; Current list of arguments and
	     ;; remaining free space.
	     (arg-list ())
	     (free usable))
	(iter (for arg :in extra-arguments)
	      (for needed = (+ *pointer-size* (length arg) 1))
	      (when (and (> needed free) arg-list)
		;; Can't add more arguments.
		;; Run the program.
		(let ((x (program-exit-status
			  (%execute-program
			   program (append arguments (nreverse arg-list))
			   :input input
			   :if-input-does-not-exist if-input-does-not-exist
			   :output output
			   :if-output-exists if-output-exists
			   :error error
			   :if-error-exists if-error-exists
			   :wait t))))
		  (unless (zerop x)
		    (return-from execute-program x)))
		;; Future invocations.
		(when (eq if-output-exists :supersede)
		  (setf if-output-exists :append))
		(when (eq if-error-exists :supersede)
		  (setf if-error-exists :append))
		;; Reset list of arguments.
		(setf arg-list ()
		      free usable))
	      ;; Add next argument.
	      (push arg arg-list)
	      (decf free needed))
	;; At this point ARG-LIST has at least one element.
	(setf arguments (append arguments (nreverse arg-list))))))
  ;; First/final invocation.
  (let ((p (%execute-program
	    program arguments
	    :input input
	    :if-input-does-not-exist if-input-does-not-exist
	    :output output
	    :if-output-exists if-output-exists
	    :error error
	    :if-error-exists if-error-exists
	    :wait (not (null wait)))))
    (if wait (program-exit-status p) p)))

;;;; Syntactic sugar.

(export 'with-input-from-program)
(defmacro with-input-from-program ((var program &optional arguments &key input (if-input-does-not-exist :error) error (if-error-exists :error)) &body body)
  (let ((p (gensym)))
    `(let* ((,p	(execute-program
		 ,program ,arguments
		 :input ,input
		 :if-input-does-not-exist ,if-input-does-not-exist
		 :output :stream
		 :error ,error
		 :if-error-exists ,if-error-exists
		 :wait nil))
	    (,var (when ,p (program-output-stream ,p))))
       (unwind-protect
	    (progn ,@body)
	 (ignore-errors (close ,var))))))

(export 'with-output-to-program)
(defmacro with-output-to-program ((var program &optional arguments &key output (if-output-exists :error) error (if-error-exists :error)) &body body)
  (let ((p (gensym)))
    `(let* ((,p (execute-program
		 ,program ,arguments
		 :input :stream
		 :output ,output
		 :if-output-exists ,if-output-exists
		 :error ,error
		 :if-error-exists ,if-error-exists
		 :wait nil))
	    (,var (when ,p (program-input-stream ,p))))
       (unwind-protect
	    (progn ,@body)
	 (ignore-errors (close ,var))))))

(export 'with-program-io)
(defmacro with-program-io ((var program &optional arguments &key error (if-error-exists :error)) &body body)
  (let ((p (gensym))
	(in (gensym))
	(out (gensym)))
    `(let* ((,p (execute-program
		 ,program ,arguments
		 :input :stream
		 :output :stream
		 :error ,error
		 :if-error-exists ,if-error-exists
		 :wait nil))
	    (,in (when ,p (program-input-stream ,p)))
	    (,out (when ,p (program-output-stream ,p)))
	    ;; The Lisp process reads from the process' output stream
	    ;; and writes to the process' input stream.
	    (,var (when ,p (make-two-way-stream ,out ,in))))
       (unwind-protect
	    (progn ,@body)
	 (ignore-errors (close ,in))
	 (ignore-errors (close ,out))
	 (ignore-errors (close ,var))))))

(export 'close-program-streams)
(defun close-program-streams (self)
  "Close all streams of an external program."
  (when self
    (flet ((%close (stream)
	     (when (and stream (open-stream-p stream))
	       (close stream))))
      (%close (program-input-stream self))
      (%close (program-output-stream self))
      (%close (program-error-stream self))))
  (values))

;;; exec.lisp ends here
