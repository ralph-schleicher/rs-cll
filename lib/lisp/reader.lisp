;;; reader.lisp --- Lisp reader.

;; Copyright (C) 2013 Ralph Schleicher

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

(defun q-reader (stream sub-char arg)
  "Read a quoted text.

First character is the delimiting character.  Non-bracketing
delimiters use the same character fore and aft.  Round brackets,
angle brackets, square brackets, and curly brackets always match
the other character of the pair.

Without prefix argument, bracketing delimiters nest.  If prefix
argument is zero, bracketing delimiters use the same character
fore and aft, that means they are treated like a non-bracketing
delimiting character.  If prefix argument is a postive number,
bracketing delimiters do not nest, that means the first matching
other character of the pair terminates reading.

You can enable this feature by evaluating the form

     (set-dispatch-macro-character #\\# #\\q #'q-reader)

After that, you can read literal strings via

     #q|He said \"Quote me!\"|
or
     #q<<tag attr=\"val\">text</tag>>
but
     #q(\")\")

will fail, because the q-reader function is not a parser for any
particular grammar."
  (declare (ignore sub-char))
  (let* ((start-char (read-char stream))
	 (end-char start-char))
    (when (or (null arg) (> arg 0))
      (cond ((char= start-char #\() (setf end-char #\)))
	    ((char= start-char #\)) (setf end-char #\())
	    ((char= start-char #\<) (setf end-char #\>))
	    ((char= start-char #\>) (setf end-char #\<))
	    ((char= start-char #\[) (setf end-char #\]))
	    ((char= start-char #\]) (setf end-char #\[))
	    ((char= start-char #\{) (setf end-char #\}))
	    ((char= start-char #\}) (setf end-char #\{))))
    (with-output-to-string (string)
      (if (and (char/= start-char end-char) (null arg))
	  (iter (with nesting-level = 1)
		(for char = (read-char stream))
		(cond ((char= char start-char)
		       (incf nesting-level))
		      ((char= char end-char)
		       (decf nesting-level)))
		(while (> nesting-level 0))
		(write-char char string))
	(iter (for char = (read-char stream))
	      (while (char/= char end-char))
	      (write-char char string))))))

;;; reader.lisp ends here
