;;; iso-639.lisp --- codes for the representation of names of languages.

;; Copyright (C) 2014 Ralph Schleicher

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

(in-package :common-lisp-user)

(defpackage :iso-639
  (:use :common-lisp
	:alexandria
	:iterate)
  (:documentation "Codes for the representation of names of languages."))

(in-package :iso-639)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (set-dispatch-macro-character #\# #\U #'rs:unicode-string-reader))

(defclass language ()
  ((part-3
    :documentation "The ISO 639-3 three-letter alphabetic code (required).")
   (part-2/b
    :initform nil
    :documentation "The ISO 639-2 three-letter alphabetic bibliographic code.")
   (part-2/t
    :initform nil
    :documentation "The ISO 639-2 three-letter alphabetic terminological code.")
   (part-1
    :initform nil
    :documentation "The ISO 639-1 two-letter alphabetic code.")
   (short-name
    :documentation "The language's short name."))
  (:documentation "A language object."))

(defvar language-database (make-hash-table)
  "Database of language objects (a hash table).
Hash key is a positive integral number.")

(defun language-key (language-code)
  "Return the hash key for language code LANGUAGE-CODE.
Argument LANGUAGE-CODE is a language code designator.
Second return value is the length of the language code
designator."
  (labels ((value (char)
	     "Return the numerical value for digit character CHAR."
	     (when-let (p (position char "ABCDEFGHIJKLMNOPQRSTUVWXYZ" :test #'char-equal))
	       (1+ p)))
	   (hash-key (string)
	     "Calculate the hash key for STRING."
	     (case (length string)
	       (2
		(when-let ((a (value (aref string 0)))
			   (b (value (aref string 1))))
		  (values (+ (* a 26) b)
			  2)))
	       (3
		(when-let ((a (value (aref string 0)))
			   (b (value (aref string 1)))
			   (c (value (aref string 2))))
		  (values (+ (* (+ (* a 26) b) 26) c)
			  3))))))
    (etypecase language-code
      (string
       (hash-key language-code))
      (symbol
       (hash-key (symbol-name language-code)))
      )))

(defun language-object (language)
  "Resolve a language object.

Argument LANGUAGE is either a language object or a language code
 designator.

Value is the language object associated with the language code,
or nil if the language is not defined."
  (if (typep language 'language)
      language
    (when-let (key (language-key language))
      (values (gethash key language-database)))))

(defun ensure-language (language)
  "Return a language object.

Argument LANGUAGE is either a language object or a language code
designator.

Signal an error if the language is not defined."
  (if (typep language 'language)
      language
    (if-let (key (language-key language))
	(or (gethash key language-database)
	    (error "Unknown language code '~A'." language))
      (error "Invalid language code."))))

(defmacro define-language (part-3 options)
  "Add a language to the database."
  (let ((canonical-options (%canonize-define-language-options options)))
    `(%define-language :part-3 ,part-3 ,@canonical-options)))

(defun %canonize-define-language-options (options)
  (let (canonical-options)
    (dolist (option options)
      (unless (listp option)
	(error "Invalid option '~S'." option))
      (ecase (first option)
	((:part-2/b :part-2/t :part-1)
	 (push option canonical-options))
	((:short-name)
	 (let ((arg (plist-alist (rest option))))
	   (push `(:short-name ',arg) canonical-options)))
	))
    (apply #'nconc (nreverse canonical-options))))

(defun %define-language (&key part-3 part-2/b part-2/t part-1 short-name)
  (let ((language (make-instance 'language)))
    ;; The ISO 639-3 three-letter alphabetic code (required).
    (unless (and (stringp part-3)
		 (= (length part-3) 3)
		 (language-key part-3))
      (error "Invalid ISO 639-3 language code."))
    (setf (slot-value language 'part-3)
	  (string-downcase part-3))
    ;; The ISO 639-2 three-letter alphabetic bibliographic code.
    (when part-2/b
      (unless (and (stringp part-2/b)
		   (= (length part-2/b) 3)
		   (language-key part-2/b))
	(error "Invalid ISO 639-2/B language code."))
      (if (string-equal part-2/b (slot-value language 'part-3))
	  (setf part-2/b nil)
	(setf (slot-value language 'part-2/b)
	      (string-downcase part-2/b))))
    ;; The ISO 639-2 three-letter alphabetic terminological code.
    (when part-2/t
      (unless (and (stringp part-2/t)
		   (= (length part-2/t) 3)
		   (language-key part-2/t))
	(error "Invalid ISO 639-2/T language code."))
      (if (or (string-equal part-2/t (slot-value language 'part-2/b))
	      (string-equal part-2/t (slot-value language 'part-3)))
	  (setf part-2/t nil)
	(setf (slot-value language 'part-2/t)
	      (string-downcase part-2/t))))
    ;; The ISO 639-1 two-letter alphabetic code.
    (when part-1
      (unless (and (stringp part-1)
		   (= (length part-1) 2)
		   (language-key part-1))
	(error "Invalid ISO 639-1 language code."))
      (setf (slot-value language 'part-1)
	    (string-downcase part-1)))
    ;; The language's short name.
    (let ((short-name (iter (for (code . name) :in short-name)
			    (for key = (language-key code))
			    (unless (and key (stringp name))
			      (error "Invalid language name."))
			    (collect (cons key name)))))
      (unless (assoc (language-key :eng) short-name)
	(error "Missing English language name."))
      (setf (slot-value language 'short-name) short-name))
    ;; Check if any language code is already in use.
    (dolist (language-code (list part-3 part-2/b part-2/t part-1))
      (when language-code
	(when (language-object language-code)
	  (error "Attempt to redefine language code '~A'." language-code))
	(when-let (key (language-key language-code))
	  (setf (gethash key language-database) language))))
    ;; No value.
    (values)))

;;;; Access functions.

(export 'iso-639-3-code)
(defun iso-639-3-code (object)
  (when-let (self (language-object object))
    (slot-value self 'part-3)))

(export 'iso-639-2/b-code)
(defun iso-639-2/b-code (object)
  (when-let (self (language-object object))
    (or (slot-value self 'part-2/b)
	(slot-value self 'part-3))))

(export 'iso-639-2/t-code)
(defun iso-639-2/t-code (object)
  (when-let (self (language-object object))
    (or (slot-value self 'part-2/t)
	(slot-value self 'part-3))))

(export 'iso-639-1-code)
(defun iso-639-1-code (object)
  (when-let (self (language-object object))
    (slot-value self 'part-1)))

(export 'language-name)
(defun language-name (object &key language)
  (when-let* ((self (language-object object))
	      (languages (ensure-list (or language :eng)))
	      (name (slot-value self 'short-name)))
    (dolist (language languages)
      (when-let* ((code (if (eq language t)
			    (slot-value self 'part-3)
			  (iso-639-3-code language)))
		  (key (language-key code))
		  (cell (assoc key name)))
	(return (cdr cell))))))

;;; iso-639.lisp ends here
