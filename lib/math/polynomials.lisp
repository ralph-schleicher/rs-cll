;;; polynomials.lisp --- evaluate and solve polynomials.

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

(defun evaluate-polynomial (coefficients number)
  "Evaluate a polynomial.

First argument COEFFICIENTS is a sequence whose elements are
 the coefficients of the polynomial in descending order.
Second argument NUMBER is the argument at which the polynomial
 is evaluated."
  (let ((value 0))
    (cond ((listp coefficients)
	   (setf value (or (first coefficients) 0))
	   (loop :for coeff :in (rest coefficients)
		 :do (fmaf value number coeff)))
	  (t
	   (let ((n (length coefficients)))
	     (when (> n 0)
	       (setf value (aref coefficients 0))
	       (loop :for j :from 1 :below n
		     :for coeff = (aref coefficients j)
		     :do (fmaf value number coeff))))))
    value))

(defun quadratic-formula-1 (p q)
  "Calculate the roots of a monic quadratic function

     f(x) = x² + p·x + q

Arguments P and Q are the coefficients of the polynomial.

Value is a list of two numbers."
  (declare (type real p q))
  (let* ((u (- (/ p 2)))
	 (v (square-root (- (square u) q))))
    (list (+ u v)
	  (- u v))))

(defun quadratic-formula (a b c)
  "Calculate the roots of a general quadratic function

     f(x) = a·x² + b·x + c

Arguments A, B, and C are the coefficients of the polynomial.

Value is a list of two numbers."
  (declare (type real a b c))
  (cond ((zerop a)
	 (error (make-condition 'division-by-zero
				:operation 'quadratic-formula
				:operands (list a b c))))
	((= a -1)
	 (setf b (- b)
	       c (- c)))
	((/= a 1)
	 (setf b (/ b a)
	       c (/ c a))))
  (quadratic-formula-1 b c))

;; http://mathworld.wolfram.com/CubicFormula.html is somewhat chaotic but
;; http://mysite.verizon.net/res148h4j/javascript/script_exact_cubic.html
;; is nice but harder to find.
(defun cubic-formula-1 (p q r)
  "Calculate the roots of a monic cubic function

     f(x) = x³ + p·x² + q·x + r

Arguments P, Q, and R are the coefficients of the polynomial.

Value is a list of three numbers.
The first element is a real number."
  (let* ((f (function (lambda (x)
	      (+ (* (+ (* (+ x p) x) q) x) r))))
	 ;; Mysterious number.
	 (w (/ p 3))
	 ;; A = (3·q - p²) / 9
	 (A (- (/ q 3) (square w)))
	 ;; B = (9·p·q - 27·r - 2·p³) / 54
	 (B (- (* w (/ q 2)) (/ r 2) (cube w)))
	 ;; Polynomial discriminant.
	 (D (+ (cube A) (square B)))
	 ;; One real root of z³ + 3·A·z - 2·B = 0.
	 (z (cond ((zerop B)
		   0)
		  ((zerop A)
		   (cube-root (* 2 B)))
		  (t
		   (if (>= D 0)
		       (+ (cube-root (+ B (square-root D)))
			  (cube-root (- B (square-root D))))
		     (let ((A (abs A)))
		       (* 2 (sqrt A) (cos (/ (acos (/ B (expt A 3/2))) 3))))))))
	 ;; Back-substitute x = z - p / 3.
	 (x (- z w)))
    ;; Refine root.
    (setf x (brent f :initial-value x :rel-tol 0 :abs-tol (float-epsilon (float x))))
    (when (rationalp D)
      (let ((u (rationalize x)))
	(when (or (= u x) (zerop (funcall f u)))
	  (setf x u))))
    (cons x (quadratic-formula-1 (+ x p) (+ (* (+ x p) x) q)))))

(defun cubic-formula (a b c d)
  "Calculate the roots of a general cubic function

     f(x) = a·x³ + b·x² + c·x + d

Arguments A, B, C, and D are the coefficients of the polynomial.

Value is a list of three numbers."
  (declare (type real a b c d))
  (cond ((= a 0)
	 (error (make-condition 'division-by-zero
				:operation 'cubic-formula
				:operands (list a b c d))))
	((= a -1)
	 (setf b (- b)
	       c (- c)
	       d (- d)))
	((/= a 1)
	 (setf b (/ b a)
	       c (/ c a)
	       d (/ d a))))
  (cubic-formula-1 b c d))

;;; polynomials.lisp ends here
