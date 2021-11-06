;;; unicode.lisp --- Unicode utilities.

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

(in-package :rs-cll)

(defun string-from-octets (sequence &optional (encoding :UTF-8))
  (babel:octets-to-string
   (coerce sequence '(vector (unsigned-byte 8)))
   :encoding encoding))

(defun unicode-string-reader (stream sub-char arg)
  "Read a sequence of Unicode characters and return it as a Lisp string.

The Unicode characters are represented as a list of non-negative
integral numbers.  Prefix argument defines the Unicode encoding.
A value of 8 means UTF-8, 16 means UTF-16BE, 32 means UTF-32BE,
2 means UCS-2BE, and 4 means UCS-4BE.  Without prefix argument,
the encoding defaults to UTF-8.

You can enable this feature by evaluating the form

     (eval-when (:compile-toplevel :load-toplevel :execute)
       (set-dispatch-macro-character #\\# #\\U #'unicode-string-reader))

After that, you can read literal Unicode strings via

     #U(71 114 105 97 195 159 32 100 105 39) ;\"Griaß di\"

If the first element of the list is a string, it is discarded.
This feature can be used instead of a comment to record the
original string.  For example,

     #U(\"Griaß di'\"
        71 114 105 97 195 159 32 100 105 39)

The ‘unicode-string-reader’ function also supports a syntax for
reading Unicode code points as characters.  That is,

     #U+03B1 = (code-char #x03B1) = #\α"
  (declare (ignore sub-char))
  (ecase (read-char stream t nil t)
    (#\(
     ;; Read a list.
     (let ((encoding (ecase arg
		       ((8 nil) :UTF-8)
		       (16 :UTF-16BE)
		       (32 :UTF-32BE)
		       (2 :UCS-2BE)
		       (4 :UCS-4BE)))
	   (octets (read-delimited-list #\) stream t)))
       (when (stringp (first octets))
	 (setf octets (rest octets)))
       (string-from-octets octets encoding)))
    (#\+
     ;; Read an Unicode code point.
     (unless (null arg)
       (error "Invalid prefix argument."))
     (let (code)
       (iter (for char = (read-char stream nil nil t))
	     (when (null char)
	       (finish))
	     (alexandria:if-let ((digit (standard-digit-char-p char 16)))
		 (setf code (if (null code) digit (+ (* code 16) digit)))
	       ;; Some other character.
	       (multiple-value-bind (macro-character-p non-terminating-p)
		   (get-macro-character char)
		 (unless (or (and macro-character-p (not non-terminating-p))
			     (standard-whitespace-char-p char))
		   (error "Invalid digit character."))
		 (unread-char char stream)
		 (finish))))
       (when (null code)
	 (error "Missing character code."))
       (code-char code)))))

;;; unicode.lisp ends here
