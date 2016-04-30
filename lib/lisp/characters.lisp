;;; characters.lisp --- characters.

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

(export 'whitespace-char-p)
(defun whitespace-char-p (char)
  "Return true if CHAR is a whitespace character.

Argument CHAR has to be a character object."
  (declare (type character char))
  (or (char= char #\Space) (not (graphic-char-p char))))

(export 'blank-char-p)
(defun blank-char-p (char)
  "Return true if CHAR is a space or horizontal tab character.

Argument CHAR has to be a character object."
  (declare (type character char))
  (or (char= char #\Space) (char= char #\Tab)))

(export 'standard-alpha-char-p)
(defun standard-alpha-char-p (char)
  "Return true if CHAR is a standard alphabetic character.

Argument CHAR has to be a character object."
  (declare (type character char))
  (and (standard-char-p char)
       (alpha-char-p char)))

(export 'standard-digit-char-p)
(defun standard-digit-char-p (char &optional (radix 10))
  "Return true if CHAR is a standard digit character.

First argument CHAR has to be a character object.
Optional second argument RADIX is an integer between 2 and 36,
 inclusive.  Default is 10.

Value is the weight of CHAR as an integer, or nil."
  (declare (type character char))
  (and (standard-char-p char)
       (digit-char-p char radix)))

;;; characters.lisp ends here
