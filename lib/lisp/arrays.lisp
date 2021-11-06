;;; arrays.lisp --- arrays.

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

(defun linear-index-from-subscripts (dimensions subscripts)
  "Return the linear index corresponding to a set of subscript values.

First argument DIMENSIONS is a list of valid array dimensions.
Second argument SUBSCRIPTS is a list of valid array indices.

The return value of the ‘linear-index-from-subscripts’ function is
equal to the value of the form

     (apply #'array-row-major-index (make-array DIMENSIONS) SUBSCRIPTS)"
  (declare (type list dimensions subscripts))
  (let ((index 0) (size 1))
    (declare (type fixnum index size))
    (map nil (lambda (dimension subscript)
	       (declare (type fixnum dimension subscript))
	       (unless (< -1 subscript dimension)
		 (error "Subscript out of range"))
	       (setf index (+ index (* subscript size))
		     size (* size dimension)))
	 (reverse dimensions) (reverse subscripts))
    index))

(defun subscripts-from-linear-index (dimensions index)
  "Return the set of subscript values corresponding to a linear index.

First argument DIMENSIONS is a list of valid array dimensions.
Second argument INDEX is a valid array index.

This is the inverse function of ‘linear-index-from-subscripts’."
  (declare (type list dimensions) (type fixnum index))
  (let ((subscript 0) (size (apply #'* dimensions)))
    (declare (type fixnum subscript size))
    (mapcar (lambda (dimension)
	      (declare (type fixnum dimension))
	      (setf size (/ size dimension))
	      (prog1
		  (multiple-value-setq (subscript index)
		    (floor index size))
		(unless (< -1 subscript dimension)
		  (error "Subscript out of range"))))
	    dimensions)))

;;; arrays.lisp ends here
