;;; getopt.lisp --- parse program arguments.

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

(export 'getopt-error)
(define-condition getopt-error (diagnostic-message)
  ((option-name
    :reader getopt-error-option-name
    :initarg :option-name
    :initform nil
    :type (or null character string)))
  (:documentation "Base class for all ‘getopt’ errors.

Initial argument OPTION-NAME is the name of the option raising the
condition, or nil."))

(export 'unknown-option-name-error)
(define-condition unknown-option-name-error (getopt-error)
  ()
  (:default-initargs
   :format-control "unknown option '~A'")
  (:documentation
   "Condition for an unknown option name.

The default format control takes one argument, the name of the option."))

(export 'ambiguous-option-name-error)
(define-condition ambiguous-option-name-error (getopt-error)
  ()
  (:default-initargs
   :format-control "option '~A' is ambiguous")
  (:documentation
   "Condition for an ambiguous option name.

The default format control takes one argument, the name of the option."))

(export 'missing-option-argument-error)
(define-condition missing-option-argument-error (getopt-error)
  ()
  (:default-initargs
   :format-control "option '~A' requires an argument")
  (:documentation
   "Condition for a missing option argument.

The default format control takes one argument, the name of the option."))

(export 'superfluous-option-argument-error)
(define-condition superfluous-option-argument-error (getopt-error)
  ()
  (:default-initargs
   :format-control "option '~A' does not take an argument")
  (:documentation
   "Condition for a superfluous option argument.

The default format control takes one argument, the name of the option."))

(defun option-name-p (object)
  "Return true if OBJECT is an option name."
  (or (characterp object) (and (stringp object) (> (length object) 0))))

(deftype option-name ()
  "Type specifier for an option name."
  '(satisfies option-name-p))

(defun list-of-option-names-p (object)
  "Return true if OBJECT is a list of option names."
  (and (listp object) (every #'option-name-p object)))

(deftype list-of-option-names ()
  "Type specifier for a list of option names."
  '(satisfies list-of-option-names-p))

(defstruct (option (:predicate optionp))
  "Data structure for a program option.

Slot NAMES is a list of option names.  Characters denote short options
 and strings denote long options.
Slot KEY is the value returned by ‘getopt’ when it encounters this
 option.  If nil, ‘getopt’ returns the matching option name.  This
 is the default.
Slot ARGUMENT defines whether or not the option has an argument.
 Value is either :no, :optional, or :required.  Default is :no.
Slot ACTION defines an alternative procedure for ‘getopt’ when it
 encounters this option.  If value is a symbol, set it to the supplied
 option argument (a string) iff the option takes an argument.  If an
 optional argument is omitted, set the symbol to nil.  If the option
 does not take an argument, set the symbol to t.  If ACTION's value
 is a function, call it with two arguments.  First argument is the
 value of the KEY (see above) and the second argument is the symbol
 value as described previously."
  (names nil
   :type list-of-option-names)
  (key nil
   :type t)
  (argument :no
   :type (member :no :optional :required))
  (action nil
   :type (or symbol function)))

(defun make-options (spec)
  "Make a list of options.

Argument SPEC is an options specification, that is a list where each
 element is either an option name, or a list of option names followed
 by ‘make-option’ initial arguments."
  (iter (for elem :in spec)
	(for opt = (etypecase elem
		     (null)
		     (list
		      (if (member :names elem)
			  (apply #'make-option elem)
			(iter (for name :in elem)
			      ;; Variable REST has to be nil at the end
			      ;; of the loop.
			      (for rest :initially elem :then (cdr rest))
			      (if (option-name-p name)
				  (collect name :into names)
				(finish))
			      (finally
			       (return (apply #'make-option :names (nreverse names) rest))))))
		     (option-name
		      (make-option :names (list elem)))))
	(unless (null opt)
	  (collect opt))))

(defun make-option-table (options)
  "Make a hash table for looking up an option by name.
Argument OPTIONS is a list of options.

Value is a hash table.  Hash key is an option name or it's
abbreviation.  Hash value is the associated option, or t if
the option name is ambiguous."
  (let ((tab (make-hash-table :test #'equal)))
    ;; Check that the option names itself are disambiguous.
    (iter (for opt :in options)
	  (iter (for name :in (option-names opt))
		(for type = (etypecase name
			      (character
			       :short)
			      (string
			       :long)))
		(when (gethash name tab)
		  ;; Duplicate option name.
		  (error 'parse-error))
		(setf (gethash name tab) type)))
    ;; Define abbreviations for long options.
    (iter (for opt :in options)
	  (iter (for name :in (option-names opt))
		(when (stringp name)
		  (iter (for end :from (length name) :downto 1)
			(for abbrev = (subseq name 0 end))
			(for value = (gethash abbrev tab))
			;; If there is a --long and --longer option,
			;; then --long is not a valid abbreviation
			;; for the --longer option.
			(unless (eq value :long)
			  (setf (gethash abbrev tab) (if value t opt)))))))
    ;; Replace type marker by actual option.
    (iter (for opt :in options)
	  (iter (for name :in (option-names opt))
		(setf (gethash name tab) opt)))
    ;; Done.
    tab))

(export '(optarg optind opterr optopt unprocessed-arguments remaining-arguments))
(defclass getopt ()
  ((optarg
    :accessor optarg
    :initform nil
    :type (or null string))
   (optind
    :accessor optind
    :initform 0
    :type (integer 0))
   (opterr
    :accessor opterr
    :initarg :opterr
    :initform t)
   (optopt
    :accessor optopt
    :initarg :optopt
    :initform #\?
    :type (not null))
   (ordering
    :accessor ordering
    :initarg :ordering
    :initform nil
    :type (or null (member :permute :require :return-in-order)))
   (help
    :accessor help-option
    :initarg :help
    :initform nil
    :type (or null string))
   (options
    :accessor options
    :initform nil)
   (option-table
    :accessor option-table
    :initform nil)
   (arguments
    :accessor arguments
    :initform nil)
   (unprocessed-arguments
    :accessor unprocessed-arguments
    :initform nil)
   (remaining-arguments
    :accessor remaining-arguments
    :initform nil)
   (first-operand
    :accessor first-operand
    :initform 0
    :type (integer 0))
   (last-operand
    :accessor last-operand
    :initform 0
    :type (integer 0))
   (next-char
    :accessor next-char
    :initform nil)
    )
  (:documentation
   "Class for parsing program arguments.

Initial argument ORDERING defines how to handle options following
 operands, that is non-option arguments.  If value is :permute,
 arguments are reordered during parsing so that all operands are
 at the end.  This is the default.  If value is :require the first
 operand terminates options processing and all remaining arguments
 are treated as operands.  This is the default if the environment
 variable POSIXLY_CORRECT or _POSIX_OPTION_ORDER is set.
Initial argument HELP defines the program argument to print the help
 text.  This is used by the ‘show-help-hint-and-die’ function."))

(export 'make-getopt)
(defun make-getopt (spec &rest init-arguments)
  "Create an option parser object.

First argument SPEC is an options specification.
Remaining arguments are ‘getopt’ initial arguments.

Return value is an option parser object.

* Options Specification

An options specification is a list where each element describes an option.

Slot NAMES is a list of option names.  Characters denote short options
 and strings denote long options.
Slot KEY is the value returned by ‘getopt’ when it encounters this
 option.  If nil, ‘getopt’ returns the matching option name.  This
 is the default.
Slot ARGUMENT defines whether or not the option has an argument.
 Value is either :no, :optional, or :required.  Default is :no.
Slot ACTION defines an alternative procedure for ‘getopt’ when it
 encounters this option.  If value is a symbol, set it to the supplied
 option argument (a string) iff the option takes an argument.  If an
 optional argument is omitted, set the symbol to nil.  If the option
  does not take an argument, set the symbol to t.  If ACTION's value
 is a function, call it with two arguments.  First argument is the
 value of the KEY (see above) and the second argument is the symbol
 value as described previously."
  (let ((self (apply #'make-instance 'getopt init-arguments)))
    (unless (ordering self)
      (setf (ordering self) (if (or (environment-variable "POSIXLY_CORRECT")
				    (environment-variable "_POSIX_OPTION_ORDER"))
				:require
			      :permute)))
    (setf (options self) (make-options spec)
	  (option-table self) (make-option-table (options self))
	  (arguments self) (copy-list (program-arguments)))
    self))

(defun non-option-p (arg)
  "Return true is ARG does not look like an option."
  (or (< (length arg) 2) (char/= (aref arg 0) #\-)))

(defun skip-option (self)
  "Skip over the current argument as an option."
  (incf (optind self))
  (setf (unprocessed-arguments self) (rest (unprocessed-arguments self))))

(defun skip-operand (self)
  "Skip over the current argument as an operand (a non-option)."
  (cond ((= (first-operand self) (last-operand self))
	 ;; List of operands is empty.  Set first-operand to the
	 ;; current argument and last-operand to the next argument.
	 (setf (first-operand self) (optind self)
	       (last-operand self) (1+ (optind self))))
	((= (last-operand self) (optind self))
	 ;; Simply extend the list of operands.
	 (incf (last-operand self)))
	(t
	 ;; Move list of operands before the current argument,
	 ;; then extend the list of operands.
	 (permute-arguments self)
	 (incf (last-operand self))))
  (skip-option self))

(defun permute-arguments (self)
  (let (head-end arg-start arg-end opt-start opt-end tail-start)
    ;; Mark sub-lists.
    (if (= (first-operand self) 0)
	(setf arg-start (arguments self))
      (setf head-end (nthcdr (1- (first-operand self)) (arguments self))
	    arg-start (cdr head-end)))
    (setf arg-end (nthcdr (- (last-operand self) (first-operand self) 1) arg-start)
	  opt-start (cdr arg-end)
	  opt-end (nthcdr (- (optind self) (last-operand self) 1) opt-start)
	  tail-start (cdr opt-end))
    ;; Swap sub-lists.
    (if (null head-end)
	(setf (arguments self) opt-start)
      (setf (cdr head-end) opt-start))
    (setf (cdr opt-end) arg-start
	  (cdr arg-end) tail-start))
  ;; Adjust operand indices.
  (incf (first-operand self) (- (optind self) (last-operand self)))
  (setf (last-operand self) (optind self)))

(export 'show-help-hint-and-die)
(defun show-help-hint-and-die (self)
  (when (help-option self)
    (format *error-output*
	    "Try '~A ~A' for more information~%"
	    (program-invocation-name)
	    (help-option self)))
  (exit-failure))

(export 'getopt)
(defun getopt (self)
  "Parse program arguments."
  (labels ((err (self condition-name)
	     (say (list condition-name (optopt self)) :option-name (optopt self))
	     (show-help-hint-and-die self)))
    (let (ans)
      ;; Get in sync.
      (when (> (last-operand self) (optind self))
	(setf (last-operand self) (optind self)))
      (when (> (first-operand self) (last-operand self))
	(setf (first-operand self) (last-operand self)))
      (setf (unprocessed-arguments self)
	    (nthcdr (optind self) (arguments self)))
      (iter (while (unprocessed-arguments self))
	    (for arg = (first (unprocessed-arguments self)))
	    (setf (optarg self) nil
		  (optopt self) nil)
	    (cond ((string= arg "--")
		   ;; The special argument ‘--’ means premature end of
		   ;; options.  Skip it like an option.
		   (skip-option self)
		   ;; Skip everything else like an operand.
		   (iter (while (unprocessed-arguments self))
			 (skip-operand self)))
		  ((non-option-p arg)
		   (skip-operand self))
		  (t
		   (let (pos name opt long-option short-option group)
		     (if (char= (aref arg 1) #\-)
			 ;; This is a long option.  Option arguments can
			 ;; be specified as '--opt=arg' or '--opt arg'.
			 (setf pos (position #\= arg :start 2)
			       name (subseq arg 2 pos)
			       long-option t)
		       ;; This is a short option.  Option arguments can
		       ;; be specified as '-oarg' or '-o arg'.  Only the
		       ;; last option in a group of short options can
		       ;; take an argument.
		       (setf pos (or (next-char self) 1)
			     name (aref arg pos)
			     short-option t
			     group (> (length arg) (1+ pos))))
		     ;; Save option name.
		     (setf (optopt self) name)
		     ;; Clear short option grouping position.  Will be
		     ;; set again iff this is a short option without
		     ;; argument inside of a group of short options.
		     (setf (next-char self) nil)
		     ;; Find matching option structure.
		     (setf opt (gethash name (option-table self)))
		     (when (null opt)
		       (err self 'unknown-option-name-error))
		     (when (eq opt t)
		       (err self 'ambiguous-option-name-error))
		     (case (option-argument opt)
		       ((:optional :required)
			(cond ((or (and long-option pos)
				   (and short-option group))
			       ;; Embedded argument.
			       (setf (optarg self) (subseq arg (1+ pos)))
			       (skip-option self))
			      ((and (rest (unprocessed-arguments self))
				    (non-option-p (second (unprocessed-arguments self))))
			       ;; Separate argument.
			       (setf (optarg self) (second (unprocessed-arguments self)))
			       (skip-option self)
			       (skip-option self))
			      (t
			       ;; No argument.
			       (when (eq (option-argument opt) :required)
				 (err self 'missing-option-argument-error))
			       (skip-option self))))
		       (:no
			(when (and long-option pos)
			  (err self 'superfluous-option-argument-error))
			(if (and short-option group)
			    (setf (next-char self) (1+ pos))
			  (skip-option self))))
		     ;; Take action.
		     (let ((key (or (option-key opt)
				    (if short-option
					name
				      (find-if (lambda (o)
						 (and (stringp o)
						      (string= o name :end1 (length name))))
					       (option-names opt)))))
			   (val (if (eq (option-argument opt) :no)
				    t
				  (optarg self))))
		       (etypecase (option-action opt)
			 (null
			  (setf ans (values key val))
			  (finish))
			 (symbol
			  (setf (symbol-value (option-action opt)) val))
			 (function
			  (funcall (option-action opt) key val))))
		     nil)))
	    (finally-protected
	     (unless (unprocessed-arguments self)
	       (decf (optind self) (- (last-operand self) (first-operand self)))
	       (setf (remaining-arguments self) (nthcdr (optind self) (arguments self))))))
      ans)))

;;; getopt.lisp ends here
