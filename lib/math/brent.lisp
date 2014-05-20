;;; brent.lisp --- Brent's method.

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

(export 'brent)
(defun brent (f &key (y 0) initial-value lower-bound upper-bound (max-iter 1000) rel-tol (abs-tol 0))
  "Solve univariate function 'y = f(x)' using Brent's method.

First argument F is a function taking one numeric argument.
 Return value of F has to be a real number.
Keyword argument Y is the function value.  Default is zero.
Keyword argument INITIAL-VALUE is the initial value for the
 function argument X.
Keyword arguments LOWER-BOUND and UPPER-BOUND specify the
 interval bounds between which the root is searched.  If any
 one of these two arguments is omitted, the interval bounds
 are determined automatically around INITIAL-VALUE.
Keyword argument MAX-ITER is the maximum number of iterations
 to be performed.  Default is 1000.
Keyword argument REL-TOL is the relative tolerance.  Default
 is machine precision.
Keyword argument ABS-TOL is the absolute tolerance.  Default
 is zero.

The iteration stops if half of the interval is less than
or equal to '2 * REL-TOL * |x| + ABS-TOL / 2'.

Primary value is the argument X for which 'y = f(x)' is true.
Secondary value is the number of iterations performed; nil
means that the algorithm did not converge within the given
number of iterations."
  (macrolet ((done (root success)
	       `(return-from brent (values ,root ,success)))
	     (sign-equal (fa fb)
	       "Return true if function values FA and FB have the same sign."
	       `(eq (null (> ,fa y)) (null (> ,fb y)))))
    (let (a fa b fb c fc d e m tol p q r s iter)
      ;; Initialization.
      (ensure-type y 'real)
      (if (and lower-bound upper-bound)
	  ;; Use given interval bounds.
	  (progn
	    (ensure-type lower-bound 'real)
	    (ensure-type upper-bound 'real)
	    ;; Sort interval bounds in ascending order.
	    (setf a lower-bound
		  b upper-bound)
	    (when (> a b)
	      (rotatef a b))
	    ;; Value at lower bound.
	    (setf fa (funcall f a))
	    (ensure-type fa 'real)
	    (when (= fa y)
	      (done a 0))
	    ;; Value at upper bound.
	    (setf fb (funcall f b))
	    (ensure-type fb 'real)
	    (when (= fb y)
	      (done b 0))
	    ;; Check for opposite sign.
	    (when (sign-equal fa fb)
	      (error "Interval bounds have the same sign.")))
	;; Determine interval bounds from initial value.
	(let (x fx)
	  (ensure-type initial-value 'real)
	  (setf x initial-value
		fx (funcall f x))
	  (ensure-type fx 'real)
	  (when (= fx y)
	    (done x 0))
	  ;; Current interval.
	  (setf a x
		fa fx)
	  (setf b x
		fb fx)
	  ;; Initial distance.
	  (setf d (* (exp (- (* (float-digits* 1F0) (log 2))))
		     (if (zerop x) 1 (abs x))))
	  (setf iter most-positive-fixnum)
	  (loop
	     ;; Lower interval bound.
	     (setf a (- x d)
		   fa (funcall f a))
	     (ensure-type fa 'real)
	     (unless (sign-equal fa fb)
	       (return))
	     ;; Upper interval bound.
	     (setf b (+ x d)
		   fb (funcall f b))
	     (ensure-type fb 'real)
	     (unless (sign-equal fa fb)
	       (return))
	     ;; Enlarge distance for the next iteration.
	     (mulf d 2)
	     (decf iter)
	     (when (= iter 0)
	       (done x nil)))))
      ;; Start setup.
      (setf c b
	    fc fb)
      (setf d (- b a)
	    e d)
      ;; Iteration.
      (if rel-tol
	  (ensure-type rel-tol '(real 0))
	(setf rel-tol (float-epsilon (float fb))))
      (ensure-type abs-tol '(real 0))
      ;; Number of remaining iterations.  If ITER is zero,
      ;; the algorithm did not converge.  Presume that we
      ;; find a solution.
      ;;
      ;; Maximum number of iterations shall be at least 1.
      (ensure-type max-iter '(integer 1))
      (setf iter max-iter)
      (loop
	 ;; Adjust interval bounds.
	 (when (sign-equal fb fc)
	   (setf c a
		 fc fa)
	   (setf d (- b a)
		 e d))
	 (when (< (abs fc) (abs fb))
	   (setf a b
		 fa fb)
	   (setf b c
		 fb fc)
	   (setf c a
		 fc fa))
	 ;; Half interval width.
	 (setf m (/ (- c b) 2))
	 ;; Convergence check.
	 ;;
	 ;; Netlib (zeroin.f) uses four times machine precision
	 ;; and a user defined absolute tolerance:
	 ;; tol1 = 2.0d0 * eps * dabs(b) + 0.5d0*tol
	 ;; xm = 0.5d0 * (c-b)
	 ;; if (dabs(xm).le.tol1) ...
	 ;;
	 ;; Matlab (fzero.m):
	 ;; m = 0.5 * (c - b);
	 ;; toler = 2.0 * tol * max(abs(b), 1.0);
	 ;; if abs(m) <= toler) ...
	 ;;
	 ;; GSL (brent.c) uses machine precision:
	 ;; tol = 0.5 * GSL_DBL_EPSILON * fabs (b);
	 ;; m = 0.5 * (c - b);
	 ;; if (fabs (m) <= tol) ...
	 ;;
	 ;; We are fully customizable.  The default is equal to
	 ;; Netlib.
	 (setf tol (+ (* 2 rel-tol (abs b)) (/ abs-tol 2)))
	 (when (or (= fb y) (<= (abs m) tol))
	   (done b (- max-iter iter)))
	 ;; Find better root.
	 (if (or (< (abs e) tol) (<= (abs fa) (abs fb)))
	     ;; Bisection.
	     (setq d m
		   e m)
	     ;; Interpolation.
	     (progn
	       (setf s (/ fb fa))
	       (if (= a c)
		   ;; Linear interpolation.
		   (setf p (* 2 m s)
			 q (- 1 s))
		 ;; Inverse quadratic interpolation.
		 (progn
		   (setf q (/ fa fc)
			 r (/ fb fc))
		   (setf p (* s (- (* 2 m q (- q r))
				   (* (- b a) (- r 1))))
			 q (* (- q 1) (- r 1)(- s 1)))))
	       (when (> p 0)
		 (setf q (- q)))
	       (setf p (abs p))
	       (if (< (* 2 p) (min (- (* 3 m q) (abs (* tol q))) (abs (* e q))))
		   (setf e d
			 d (/ p q))
		 ;; Interpolation failed, fall back to bisection.
		 (setf d m
		       e m))))
	 ;; Next point.
	 (setf a b
	       fa fb)
	 (cond ((> (abs d) tol)
		(incf b d))
	       ((> m 0)
		(incf b tol))
	       (t
		(decf b tol)))
	 (decf iter)
	 (when (= iter 0)
	   (done b nil))
	 (setf fb (funcall f b))
	 (ensure-type fb 'real)
	 ))))

;;; brent.lisp ends here
