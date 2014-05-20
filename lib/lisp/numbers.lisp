;;; numbers.lisp --- numbers.

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

(export 'pi/6)
(defconst pi/6 (/ pi 6)
  "One sixth of the ratio of a circle's circumference to its diameter.
This is equal to 30 arc degree.")

(export 'pi/4)
(defconst pi/4 (/ pi 4)
  "One quarter of the ratio of a circle's circumference to its diameter.
This is equal to 45 arc degree.")

(export 'pi/2)
(defconst pi/2 (/ pi 2)
  "One half of the ratio of a circle's circumference to its diameter.
This is equal to 90 arc degree.")

(export '2*pi)
(defconst 2*pi (* 2 pi)
  "Two times the ratio of a circle's circumference to its diameter.
This is equal to 360 arc degree.")

(export 'minf)
(defmacro minf (place &rest numbers &environment env)
  "Set the value designated by PLACE to the minimum
of the current value and NUMBERS."
  (multiple-value-bind (temp-var temp-val new-var set-form get-form)
      (get-setf-expansion place env)
    `(let* (,@(mapcar #'list temp-var temp-val)
	    (,@new-var (min ,get-form ,@numbers)))
       ,set-form)))

(export 'maxf)
(defmacro maxf (place &rest numbers &environment env)
  "Set the value designated by PLACE to the maximum
of the current value and NUMBERS."
  (multiple-value-bind (temp-var temp-val new-var set-form get-form)
      (get-setf-expansion place env)
    `(let* (,@(mapcar #'list temp-var temp-val)
	    (,@new-var (max ,get-form ,@numbers)))
       ,set-form)))

(export 'addf)
(defmacro addf (place &rest numbers &environment env)
  "Add NUMBERS to the value designated by PLACE."
  (multiple-value-bind (temp-var temp-val new-var set-form get-form)
      (get-setf-expansion place env)
    `(let* (,@(mapcar #'list temp-var temp-val)
	    (,@new-var (+ ,get-form ,@numbers)))
       ,set-form)))

(export 'subf)
(defmacro subf (place &rest numbers &environment env)
  "Subtract NUMBERS from the value designated by PLACE.
If NUMBERS is omitted, change the sign of the value."
  (multiple-value-bind (temp-var temp-val new-var set-form get-form)
      (get-setf-expansion place env)
    `(let* (,@(mapcar #'list temp-var temp-val)
	    (,@new-var (- ,get-form ,@numbers)))
       ,set-form)))

(export 'mulf)
(defmacro mulf (place &rest numbers &environment env)
  "Multiply the value designated by PLACE by NUMBERS."
  (multiple-value-bind (temp-var temp-val new-var set-form get-form)
      (get-setf-expansion place env)
    `(let* (,@(mapcar #'list temp-var temp-val)
	    (,@new-var (* ,get-form ,@numbers)))
       ,set-form)))

(export 'divf)
(defmacro divf (place &rest numbers &environment env)
  "Divide the value designated by PLACE by NUMBERS.
If NUMBERS is omitted, invert the value."
  (multiple-value-bind (temp-var temp-val new-var set-form get-form)
      (get-setf-expansion place env)
    `(let* (,@(mapcar #'list temp-var temp-val)
	    (,@new-var (/ ,get-form ,@numbers)))
       ,set-form)))

(export 'fma)
(defsubst fma (number multiplicand summand)
  "Multiply NUMBER by MULTIPLICAND, then add SUMMAND.
Attempt to perform a fused multiply-add operation.

This is the inverse of the `fsd' function."
  (+ (* number multiplicand) summand))

(export 'fsd)
(defsubst fsd (number subtrahend divisor)
  "Subtract SUBTRAHEND from NUMBER, then divide by DIVISOR.
Attempt to perform a fused subtract-divide operation.

This is the inverse of the `fma' function."
  (/ (- number subtrahend) divisor))

(export 'fmaf)
(defmacro fmaf (place multiplicand summand &environment env)
  "Multiply the value designated by PLACE by MULTIPLICAND, then add SUMMAND.
Attempt to perform a fused multiply-add operation."
  (multiple-value-bind (temp-var temp-val new-var set-form get-form)
      (get-setf-expansion place env)
    `(let* (,@(mapcar #'list temp-var temp-val)
	    (,@new-var (fma ,get-form ,multiplicand ,summand)))
       ,set-form)))

(export 'fsdf)
(defmacro fsdf (place subtrahend divisor &environment env)
  "Subtract SUBTRAHEND from the value designated by PLACE, then divide by DIVISOR.
Attempt to perform a fused subtract-divide operation."
  (multiple-value-bind (temp-var temp-val new-var set-form get-form)
      (get-setf-expansion place env)
    `(let* (,@(mapcar #'list temp-var temp-val)
	    (,@new-var (fsd ,get-form ,subtrahend ,divisor)))
       ,set-form)))

(export 'float-epsilon)
(defsubst float-epsilon (&optional (float 1F0))
  "Return the smallest positive floating-point number
used in the representation of FLOAT, such that the
expression

     (= (float 1 EPSILON) (+ (float 1 EPSILON) EPSILON))

is false."
  (etypecase float
    (short-float
     short-float-epsilon)
    (single-float
     single-float-epsilon)
    (double-float
     double-float-epsilon)
    (long-float
     long-float-epsilon)))

(export 'float-negative-epsilon)
(defsubst float-negative-epsilon (&optional (float 1F0))
  "Return the smallest positive floating-point number
used in the representation of FLOAT, such that the
expression

     (= (float 1 EPSILON) (- (float 1 EPSILON) EPSILON))

is false."
  (etypecase float
    (short-float
     short-float-negative-epsilon)
    (single-float
     single-float-negative-epsilon)
    (double-float
     double-float-negative-epsilon)
    (long-float
     long-float-negative-epsilon)))

(export 'float-digits*)
(defun float-digits* (float &optional (base 10))
  "Return the number of digits used in the representation of FLOAT.
This includes any implicit digits.

Optional argument BASE is the radix for the return value.
 Default is 10.

If BASE is equal to the radix of FLOAT, value is an integral
number.  Otherwise, value is a floating-point number in the
format of FLOAT."
  (declare (type (integer 2) base))
  (* (float-digits float)
     (if (/= (float-radix float) base)
	 (log (float (float-radix float) float) base)
       1)))
#-(and)
(mapcar #'float-digits* '(1.0E0 1.0S0 1.0F0 1.0D0 1.0L0))

(export 'float-precision*)
(defun float-precision* (float &optional (base 10))
  "Return the number of significant digits present in FLOAT.

Optional argument BASE is the radix for the return value.
 Default is 10.

If BASE is equal to the radix of FLOAT, value is an integral
number.  Otherwise, value is a floating-point number in the
format of FLOAT.  If FLOAT is zero, value is zero."
  (declare (type (integer 2) base))
  (* (float-precision float)
     (if (/= (float-radix float) base)
	 (log (float (float-radix float) float) base)
       1)))
#-(and)
(mapcar #'float-precision* '(1.0E0 1.0S0 1.0F0 1.0D0 1.0L0 0.0))

(export 'decimal-digits)
(defun decimal-digits (float)
  "Return the number of decimal digits needed to preserve the original
floating-point number when converting it to a decimal character format."
  (1+ (ceiling (* (float-precision float) (log (float-radix float) 10)))))
#-(and)
(mapcar #'decimal-digits '(1.0E0 1.0S0 1.0F0 1.0D0 1.0L0))

(export 'absolute-ascending)
(defun absolute-ascending (a b)
  "Return true if the absolute value of number A is less than the
absolute value of number B.  This function can be used to sort
numbers in ascending order."
  (< (abs a) (abs b)))

(export 'absolute-descending)
(defun absolute-descending (a b)
  "Return true if the absolute value of number A is greater than the
absolute value of number B.  This function can be used to sort
numbers in descending order."
  (> (abs a) (abs b)))

(export 'roman-numeral)
(defun roman-numeral (n)
  "Convert the integral number N into a roman number (a string).
If N is zero, the return value is nil.  If N is a negative number,
utilize lowercase letters.  Otherwise, use uppercase letters."
  (declare (type integer n))
  (unless (zerop n)
    (let ((str (format nil "~@R" (abs n))))
      (when (minusp n)
	(nstring-downcase str))
      str)))

;;; numbers.lisp ends here
