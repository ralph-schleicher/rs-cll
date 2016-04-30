;;; types.lisp --- types and classes.

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

(export 'list-of-strings-p)
(defun list-of-strings-p (object)
  "Return true if OBJECT is a list of strings.

If OBJECT is the empty list, value is true, too."
  (and (listp object) (every #'stringp object)))

(export 'list-of-strings)
(deftype list-of-strings ()
  "Type specifier for a list of strings."
  '(satisfies list-of-strings-p))

(defun type-size (type-spec)
  "Attempt to determine the size of a type.

Argument TYPE-SPEC has to be a valid type specifier.

Value is the size of the type in byte, or nil if the size can not
be determined.

The size only can be determined if TYPE-SPEC is a fully specified
compound type specifier.  For example,

     (unsigned-byte 8)
     (integer 0 255)
     (mod 256)

are fully specified compound type specifiers for an unsigned byte."
  #-(and)
  (typep t type-spec)
  (when (and (subtypep type-spec 'integer) (consp type-spec))
    (let ((type (first type-spec)))
      (cond ((or (eq type 'unsigned-byte) (eq type 'signed-byte))
	     (let ((s (second type-spec)))
	       (when (integerp s)
		 (ceiling (/ s 8)))))
	    ((eq type 'integer)
	     (let ((low (second type-spec))
		   (high (third type-spec)))
	       (when (consp low)
		 (setf low (first low))
		 (when (integerp low)
		   (incf low)))
	       (when (consp high)
		 (setf high (first high))
		 (when (integerp high)
		   (decf high)))
	       (when (and (integerp low) (integerp high))
		 (let ((s (+ (max (integer-length low)
				  (integer-length high))
			     (if (or (< low 0) (< high 0)) 1 0))))
		   (ceiling (/ s 8))))))
	    ((eq type 'mod)
	     (let* ((n (second type-spec))
		    (s (round (log n 2))))
	       (when (= (expt 2 s) n)
		 (ceiling (/ s 8)))))
	    ))))

;;; types.lisp ends here
