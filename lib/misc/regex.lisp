;;; regex.lisp --- regular expression utilities.

;; Copyright (C) 2012, 2013 Ralph Schleicher

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

(defvar *regex-cache* (make-hash-table :test #'equal)
  "Cache for compiled regular expressions.")

(defsubst get-regex (regexp)
  "Return compiled regular expression for REGEXP."
  (if (functionp regexp)
      regexp
    (multiple-value-bind (value exists)
	(gethash regexp *regex-cache*)
      (if exists
	  value
	(setf (gethash regexp *regex-cache*)
	      (cl-ppcre:create-scanner regexp))))))

(defun make-match-data (&optional (n 0))
  "Create a match data object."
  (declare (type (integer 0) n))
  (make-array n :initial-element nil :adjustable t))

(defmacro adjust-match-data (self n &environment env)
  "Resize a match data object."
  (multiple-value-bind (temp-var temp-val new-var set-form get-form)
      (get-setf-expansion self env)
    `(let* (,@(mapcar #'list temp-var temp-val)
	    (,@new-var (adjust-array ,get-form ,n :initial-element nil)))
       ,set-form)))

(defun copy-match-data (self)
  "Copy a match data object."
  (let* ((n (length self))
	 (a (make-match-data n)))
    (iter (for j :from 0 :below n)
	  (for elem = (aref self j))
	  (when elem
	    (for (start . end) = elem)
	    (setf (aref a j) (cons start end))))
    a))

(defvar *last-thing-searched* nil
  "The object in which the last search was performed.
Value is nil if no searching has been done yet.")

(defvar *match-data* (make-match-data)
  "The latest match data (a vector).
Elements are either bounding indices or nil.
Bounding indices are cons cells of the form

     (START . END)

where START is the start index and END is the end index of
the corresponding parenthesized expression.")

(defun set-match-data (string match-start match-end group-start group-end)
  "Update global variables from ‘cl-ppcre:scan’ arguments/values."
  (setf *last-thing-searched* string)
  (macrolet ((set-match (elem start end)
	       `(if (null ,start)
		    ;; No match, clear ELEM.
		    (when ,elem
		      (setf ,elem nil))
		  (if (null ,elem)
		      ;; Set bounding indices.
		      (setf ,elem (cons ,start ,end))
		    (progn
		      ;; Replace bounding indices.
		      (rplaca ,elem ,start)
		      (rplacd ,elem ,end))))))
    (let ((n (length group-start)))
      (adjust-match-data *match-data* (1+ n))
      (set-match (aref *match-data* 0)
		 match-start
		 match-end)
      (iter (for subexp :from 1 :to n)
	    (for group :from 0)
	    (set-match (aref *match-data* subexp)
		       (aref group-start group)
		       (aref group-end group))))))

(export 'string-match)
(defun string-match (regexp string &key (start 0) end)
  "Return start position of first match for REGEXP in STRING, or nil
if there is no match.

First argument REGEXP is a regular expression.
Second argument STRING is the target string.
Keyword arguments START and END are bounding indices in STRING.
 Default values are zero and the length of STRING.

See the ‘cl-ppcre:scan’ function, for more details."
  (multiple-value-bind (match-start match-end group-start group-end)
      (cl-ppcre:scan (get-regex regexp) string :start start :end (or end (length string)))
    (set-match-data string match-start match-end group-start group-end)
    (values match-start match-end group-start group-end)))

(defsubst %match-start (subexp)
  "Return start index of text matched by last search."
  (declare (type (integer 0) subexp))
  (car (aref *match-data* subexp)))

(defsubst %match-end (subexp)
  "Return end index of text matched by last search."
  (declare (type (integer 0) subexp))
  (cdr (aref *match-data* subexp)))

(export 'match-start)
(defun match-start (&optional (subexp 0))
  "Return start index of text matched by last search.

Optional argument SUBEXP (a non-negative integer) specifies a
 parenthesized expression.  A value of zero means the entire
 match.  This is the default.

Value is nil if there is no match."
  (declare (type (integer 0) subexp))
  (when (< subexp (length *match-data*))
    (%match-start subexp)))

(export 'match-end)
(defun match-end (&optional (subexp 0))
  "Return end index of text matched by last search.

Optional argument SUBEXP (a non-negative integer) specifies a
 parenthesized expression.  A value of zero means the entire
 match.  This is the default.

Value is nil if there was no match."
  (declare (type (integer 0) subexp))
  (when (< subexp (length *match-data*))
    (%match-end subexp)))

(export 'match-data)
(defun match-data ()
  "Return list of bounding indices on what the last search matched."
  (iter (for (start . end) :in-vector *match-data*)
	(collect start)
	(collect end)))

(export 'save-match-data)
(defmacro save-match-data (&body body)
  "Save match data, execute BODY forms, restore match data.
Value is the value of the last form in BODY."
  `(let ((*last-thing-searched* *last-thing-searched*)
	 (*match-data* (copy-match-data *match-data*)))
     (unwind-protect (progn ,@body))))

(defsubst %match-string (subexp)
  "Return matched text, or nil if SUBEXP does not match."
  (declare (type (integer 0) subexp))
  (let ((elem (aref *match-data* subexp)))
    (and elem (subseq *last-thing-searched* (car elem) (cdr elem)))))

(export 'match-string)
(defun match-string (&optional (subexp 0))
  "Return string of text matched by last search.

Optional argument SUBEXP (a non-negative integer) specifies a
 parenthesized expression.  A value of zero means the entire
 match.  This is the default.

Value is nil if there is no match."
  (declare (type (integer 0) subexp))
  (when (< subexp (length *match-data*))
    (%match-string subexp)))

(export 'match-strings)
(defun match-strings ()
  "Return strings of text matched by last search.

Value is a list.  A list element of nil means that the corresponding
parenthesized expression did not match."
  (iter (for subexp :from 0 :below (length *match-data*))
	(collect (%match-string subexp))))

(export 'replace-match)
(defun replace-match (new-text &optional (subexp 0))
  "Replace text matched by last search.

Optional argument SUBEXP (a non-negative integer) specifies a
 parenthesized expression.  A value of zero means the entire
 match.  This is the default."
  (declare (type (integer 0) subexp))
  (if (< subexp (length *match-data*))
      (concatenate 'string
		   (subseq *last-thing-searched* 0 (%match-start subexp))
		   new-text
		   (subseq *last-thing-searched* (%match-end subexp)))
    *last-thing-searched*))

;;; regex.lisp ends here
