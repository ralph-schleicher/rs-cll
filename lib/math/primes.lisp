;;; primes.lisp --- prime numbers.

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

(export 'modular-exponentiation)
(defun modular-exponentiation (base exponent divisor)
  "Calculate ‘(rem (expt BASE EXPONENT) DIVISOR)’ efficiently.

First argument BASE is the base.  Value has to be a positive number.
Second argument EXPONENT is the exponent.  Value has to be a non-negative number.
Third argument DIVISOR is the divisor.  Value has to be a positive number.

Return value is the modulus."
  (declare #.optimize-for-speed)
  (check-type base (integer 1))
  (check-type exponent (integer 0))
  (check-type divisor (integer 1))
  (let ((b base)
	(e exponent)
	(d divisor))
    (cond ((= d 1)
	   0)
	  ((= e 0)
	   1)
	  (t
	   (when (>= b d)
	     (setf b (rem b d)))
	   (iter (with c = 1)
		 (with r = 0)
		 (multiple-value-setq (e r)
		   (truncate e 2))
		 (when (= r 1)
		   (setf c (rem (* c b) d)))
		 (while (> e 0))
		 (setf b (rem (* b b) d))
		 (finally
		  (return c)))))))

(export 'jacobi-symbol)
(defun jacobi-symbol (a n)
  "Calculate the Jacobi symbol ‘J(a,n) = (a/n)’.

First argument A is the ‘numerator’.  Value has to be a non-negative integer.
Second argument N is the ‘denominator’.  Value has to be a positive odd integer.

Return value is either 1, 0, or -1."
  (declare #.optimize-for-speed)
  (check-type a (integer 0))
  (check-type n (and (integer 1) (satisfies odd-integer-p)))
  (let ((j 1))
    (loop (when (>= a n)
	    (setf a (rem a n)))
	  (cond ((= a 0)
		 (when (/= n 1)
		   (setf j 0))
		 (return))
		((= a 1)
		 (return))
		((evenp a)
		 (setf a (/ a 2))
		 (when (member (rem n 8) '(3 5))
		   (setf j (- j))))
		(t
		 ;; A and N are odd.
		 (rotatef a n)
		 (when (and (= (rem a 4) 3) (= (rem n 4) 3))
		   (setf j (- j))))))
    j))

(defvar *primes-cache-size*
  (min (expt 2 19) ;64 kiB
       (if (evenp array-dimension-limit)
	   array-dimension-limit
	 (1- array-dimension-limit)))
  "Cache size.  Value has to be an even number.")
(declaim (type fixnum *primes-cache-size*))

(defvar *primes-cache*
  (let ((p (make-array *primes-cache-size* :element-type 'bit :initial-element 0)))
    ;; The only even prime number.
    (setf (sbit p 2) 1)
    ;; Loop over odd numbers; that means none of the
    ;; tested numbers can be divided by two.
    (iter (for n :from 3 :below *primes-cache-size* :by 2)
	  (iter (for d :from 3 :to (isqrt n) :by 2)
		(when (and (= (sbit p d) 1) (zerop (rem n d)))
		  (leave))
		(finally
		 ;; N is prime.
		 (setf (sbit p n) 1))))
    p)
  "Cache of prime numbers (a simple bit vector).")

(defmacro %primep (n)
  "Query the cache whether or not N is a prime number."
  `(= (sbit *primes-cache* ,n) 1))

(defun %is-prime (n)
  "Return N if it is a prime number, or nil.
Argument N has to be a non-negative integral number."
  (declare #.optimize-for-speed)
  (declare (type integer n))
  (cond ((< n *primes-cache-size*)
	 (if (%primep n) n))
	((oddp n)
	 (let ((max-divisor (isqrt n)))
	   (iter (for d :from 3 :to (min max-divisor *primes-cache-size*) :by 2)
		 (when (and (%primep d) (zerop (rem n d)))
		   (return-from %is-prime)))
	   (iter (for d :from (1+ *primes-cache-size*) :to max-divisor :by 2)
		 (when (zerop (rem n d))
		   (return-from %is-prime)))
	   ;; N is prime.
	   n))))

(defun %next-prime (n)
  "Return the prime number greater than N.
Argument N has to be an integral number."
  (declare #.optimize-for-speed)
  (declare (type integer n))
  (if (> n 1)
      ;; Start at next odd number after N.
      (iter (with k = (if (oddp n) (+ n 2) (1+ n)))
	    (when (%is-prime k)
	      (leave k))
	    (incf k 2))
    2))

(defun %previous-prime (n)
  "Return the prime number less than N, or nil.
Argument N has to be an integral number."
  (declare #.optimize-for-speed)
  (declare (type integer n))
  (cond ((> n 3)
	 ;; Start at previous odd number before N.
	 (iter (with k = (if (oddp n) (- n 2) (1- n)))
	       (when (%is-prime k)
		 (leave k))
	       (decf k 2)))
	((= n 3)
	 2)))

(export 'is-prime)
(defun is-prime (n)
  "Return N if it is a prime number, or nil."
  (declare #.optimize-for-speed)
  (when (and (integerp n) (> n 1))
    (%is-prime n)))

(export 'next-prime)
(defun next-prime (n)
  "Return the prime number greater than N, or nil."
  (declare #.optimize-for-speed)
  (when (integerp n)
    (%next-prime n)))

(export 'next-prime*)
(defun next-prime* (n)
  "Return the prime number greater than or equal to N, or nil."
  (declare #.optimize-for-speed)
  (when (integerp n)
    (if (< n 2) 2 (or (%is-prime n) (%next-prime n)))))

(export 'previous-prime)
(defun previous-prime (n)
  "Return the prime number less than N, or nil."
  (declare #.optimize-for-speed)
  (when (integerp n)
    (%previous-prime n)))

(export 'previous-prime*)
(defun previous-prime* (n)
  "Return the prime number less than or equal to N, or nil."
  (declare #.optimize-for-speed)
  (when (integerp n)
    (if (> n 1) (or (%is-prime n) (%previous-prime n)))))

(export 'primes-between)
(defun primes-between (from to)
  "Return a list of prime numbers between FROM and TO, inclusive."
  (declare #.optimize-for-speed)
  (when (and (integerp from)
	     (integerp to))
    (let (primes)
      (cond ((<= from 2 to)
	     (push 2 primes)
	     (setf from 3))
	    ((evenp from)
	     (incf from)))
      (iter (with k = from)
	    (cond ((> k to)
		   (finish))
		  ((%is-prime k)
		   (push k primes)))
	    (incf k 2))
      (nreverse primes))))

(export 'nth-prime)
(defun nth-prime (n)
  "Return the N-th prime number, or nil.

The first prime number is 2.
Argument N is one-based, i.e.

     (nth-prime 1)
      => 2"
  (declare #.optimize-for-speed)
  (when (integerp n)
    (cond ((< n 1)
	   nil)
	  ((= n 1)
	   2)
	  (t
	   (iter (for k :from 3 :by 2)
		 (when (%is-prime k)
		   (decf n)
		   (when (= n 1)
		     (leave k)))))
	  )))

(export 'prime-factors)
(defun prime-factors (n &optional primary)
  "Return a list of prime factors of N.

First argument N has to be a number (an integer).
If optional argument PRIMARY is true, value is a list of cons cells
 where the ‘car’ is the prime factor and the ‘cdr’ is it's order, i.e.
 it's exponent or repeat count.  Otherwise, value is a list of prime
 factors where higher order prime factors occur multiple times.

If N is not greater than one, value is the empty list.  If N is a
prime number, value is a list of length one.

Examples:

     (prime-factors 90)
      => (2 3 3 5)
     (prime-factors 90 t)
      => ((2 . 1) (3 . 2) (5 . 1))
     (prime-factors 7)
      => (7)
     (prime-factors 1)
      => nil"
  (declare #.optimize-for-speed)
  (declare (type integer n))
  (when (> n 1)
    (macrolet ((main (collect)
		 `(iter (with n = n)
			;; Trial divisor.
			(with d = 2)
			;; Maximum divisor.
			(with m = (isqrt n))
			(cond ((> d m)
			       ;; N is prime.
			       (,collect n)
			       (finish))
			      ;; Need quotient and remainder of the trial
			      ;; division.  If the remainder is non-zero,
			      ;; the clause fails.
			      ((multiple-value-bind (q r)
				   (truncate n d)
				 (when (zerop r)
				   (,collect d)
				   (setf n q ;(/ n d)
					 m (isqrt n)))))
			      ((> d 2)
			       (iter (incf d 2)
				     (while (< d *primes-cache-size*))
				     (until (%primep d))))
			      (t
			       (setf d 3))
			      ))))
      (if (not primary)
	  (main collect)
	(let (ans)
	  (flet ((collect* (p)
		   (if (and ans (= (caar ans) p))
		       (incf (cdar ans))
		     (push (cons p 1) ans))))
	    (main collect*))
	  (nreverse ans))
	))))

;;; primes.lisp ends here
