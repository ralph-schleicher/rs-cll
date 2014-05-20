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

(export 'primep)
(defun primep (n)
  "Return N if it is a prime number, or nil."
  (when (integerp n)
    (cond ((< n 2)
	   nil)
	  ((= n 2)
	   n)
	  ((oddp n)
	   (iter (for d :from 2 :to (isqrt n))
		 (when (zerop (rem n d))
		   (leave nil))
		 (finally
		  (return n))))
	  )))

(export 'next-prime)
(defun next-prime (n)
  "Return the prime number greater than N, or nil."
  (when (integerp n)
    (if (> n 1)
	;; Start at next odd number after N.
	(iter (with k = (if (oddp n) (+ n 2) (1+ n)))
	      (when (primep k)
		(leave k))
	      (incf k 2))
      2)))

(export 'next-prime*)
(defun next-prime* (n)
  "Return the prime number greater than or equal to N, or nil."
  (or (primep n) (next-prime n)))

(export 'previous-prime)
(defun previous-prime (n)
  "Return the prime number less than N, or nil."
  (when (integerp n)
    (cond ((> n 3)
	   ;; Start at previous odd number before N.
	   (iter (with k = (if (oddp n) (- n 2) (1- n)))
		 (when (primep k)
		   (leave k))
		 (decf k 2)))
	  ((= n 3)
	   2))))

(export 'previous-prime*)
(defun previous-prime* (n)
  "Return the prime number less than or equal to N, or nil."
  (or (primep n) (previous-prime n)))

(export 'primes-between)
(defun primes-between (from to)
  "Return a list of prime numbers between FROM and TO, inclusive."
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
		  ((primep k)
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
  (when (integerp n)
    (cond ((< n 1)
	   nil)
	  ((= n 1)
	   2)
	  (t
	   (iter (for k :from 3 :by 2)
		 (when (primep k)
		   (decf n)
		   (when (= n 1)
		     (leave k)))))
	  )))

(export 'prime-factors)
(defun prime-factors (n)
  "Return a list of prime factors of N.
If N is prime, return value is the list (N)."
  (when (and (integerp n) (> n 1))
    (iter (with n = n)
	  ;; Trial divisor.
	  (with d = 2)
	  ;; Maximum divisor.
	  (with m = (isqrt n))
	  (cond ((> d m)
		 ;; N is prime.
		 (collect n)
		 (finish))
		;; Need quotient and remainder of the trial
		;; division.  If the remainder is non-zero,
		;; the clause fails.
		((multiple-value-bind (q r)
		     (truncate n d)
		   (when (zerop r)
		     (collect d)
		     (setf n q ; (/ n d)
			   m (isqrt n)))))
		((> d 2)
		 (incf d 2))
		(t
		 (setf d 3))
		))))

;;; primes.lisp ends here
